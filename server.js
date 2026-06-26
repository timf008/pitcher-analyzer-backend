const express = require("express");
const path = require("path");
const { exec } = require("child_process");
const csv = require("csv-parser");
const app = express();
const fs = require("fs");
const cors = require("cors");

// ---------------------------
// CORS (required for GitHub Pages frontend)
// ---------------------------
app.use(cors());

// ---------------------------
// Static File Serving
// ---------------------------
app.use(express.static(path.join(__dirname, "public")));

// ---------------------------
// API: Run R script for pitcher data
// ---------------------------
app.get("/api/pitchers", (req, res) => {
    // Force CORS headers even on errors
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers", "Content-Type");
    res.header("Access-Control-Allow-Methods", "GET");

    const { name, season } = req.query;

    if (!name || !season) {
        return res.status(400).json({ error: "Missing name or season" });
    }

    const cmd = `Rscript "${path.join(__dirname, "stathead.r")}" "${name}" "${season}"`;

    exec(cmd, (error, stdout, stderr) => {
        if (error) {
            console.error("R error:", error);
            return res.status(500).json({ error: "R script failed" });
        }

        if (stderr) {
            console.error("R stderr:", stderr);
        }

        try {
            const json = JSON.parse(stdout);
            return res.json(json);
        } catch (e) {
            console.error("JSON parse error:", e);
            console.log("Raw R output:", stdout);
            return res.status(500).json({ error: "Invalid JSON from R" });
        }
    });
});


// -------------------------------------------
// API: Return list of pitchers WITH GS + ERA
// -------------------------------------------
app.get("/api/pitcherList", (req, res) => {
    const { season } = req.query;

    const filePath = path.join(__dirname, `stathead_pitching_${season}.csv`);

    const rows = [];
    fs.createReadStream(filePath)
        .pipe(csv())
        .on("data", (row) => {
            const name = row.Player;
            const id = row["Player-additional"] || null;

            const gs  = Number(row.GS);
            const era = Number(row.ERA);

            rows.push({
                name,
                id,
                GS: Number.isNaN(gs) ? null : gs,
                ERA: Number.isNaN(era) ? null : era
            });
        })
        .on("end", () => {
            res.json(rows);
        });
});


// ---------------------------
// Backend for Emoji Trend Button - Graph
// ---------------------------
app.get("/api/pitcherTrend", async (req, res) => {
    res.setHeader("Access-Control-Allow-Origin", "*");
    res.setHeader("Access-Control-Allow-Headers", "Content-Type");
    res.setHeader("Access-Control-Allow-Methods", "GET");

    const { name, stat } = req.query;
    const seasons = [2025, 2024, 2023];

    const results = [];

    for (const season of seasons) {
        const cmd = `Rscript "${path.join(__dirname, "trend.r")}" "${name}" "${stat}" ${season}`;

        try {
            const output = await new Promise((resolve, reject) => {
                exec(cmd, (error, stdout, stderr) => {
                    if (error) return resolve(null);
                    resolve(stdout);
                });
            });

            if (!output) {
                results.push({ season, value: null });
                continue;
            }

            const json = JSON.parse(output);
            let value = json.value;

            if (value && typeof value === "object" && Object.keys(value).length === 0) {
                value = null;
            }

            results.push({ season, value });

        } catch (e) {
            results.push({ season, value: null });
        }
    }

    res.json(results);
});



// --------------------------------------
// API: Last Updated timestamp for CSV
// --------------------------------------
app.get("/api/last-updated/pitchers/:season", (req, res) => {
    const season = req.params.season;

    const filePath = path.join(
        __dirname,
        `stathead_pitching_${season}.csv`
    );

    fs.stat(filePath, (err, stats) => {
        if (err) {
            console.error("Timestamp error:", err);
            return res.status(404).json({
                error: `CSV for season ${season} not found`
            });
        }

        res.json({
            season,
            lastUpdated: stats.mtime
        });
    });
});

// ---------------------------
// Start Server
// ---------------------------
const PORT = 3000;
app.listen(PORT, () => {
    console.log(`Pitcher Analyzer running at http://localhost:${PORT}`);
});
