const express = require("express");
const path = require("path");
const { exec } = require("child_process");
const csv = require("csv-parser");
const app = express();
const fs = require("fs");
const cors = require("cors");

// ---------------------------
// GLOBAL CORS (Express 5 compatible)
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
            rows.push({
                name: row.Player,
                id: row["Player-additional"] || null,
                GS: Number(row.GS) || null,
                ERA: Number(row.ERA) || null
            });
        })
        .on("end", () => res.json(rows))
        .on("error", (err) => {
            console.error("CSV read error:", err);
            res.status(500).json({ error: "CSV read failed" });
        });
});

// ---------------------------
// API: Trend data (R script)
// ---------------------------
app.get("/api/pitcherTrend", async (req, res) => {
    const { name, stat } = req.query;
    const seasons = [2025, 2024, 2023];

    const results = [];

    for (const season of seasons) {
        const cmd = `Rscript "${path.join(__dirname, "trend.r")}" "${name}" "${stat}" ${season}`;

        try {
            const output = await new Promise((resolve) => {
                exec(cmd, (error, stdout) => {
                    if (error) return resolve(null);
                    resolve(stdout);
                });
            });

            if (!output) {
                results.push({ season, value: null });
                continue;
            }

            const json = JSON.parse(output);
            const value = json.value || null;

            results.push({ season, value });

        } catch (e) {
            results.push({ season, value: null });
        }
    }

    return res.json(results);
});

// --------------------------------------
// API: Last Updated timestamp for CSV
// --------------------------------------
app.get("/api/last-updated/pitchers/:season", (req, res) => {
    const season = req.params.season;
    const filePath = path.join(__dirname, `stathead_pitching_${season}.csv`);

    fs.stat(filePath, (err, stats) => {
        if (err) {
            console.error("Timestamp error:", err);
            return res.status(404).json({ error: `CSV for season ${season} not found` });
        }

        return res.json({
            season,
            lastUpdated: stats.mtime
        });
    });
});

// ---------------------------
// Start Server
// ---------------------------
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`Pitcher Analyzer running at http://localhost:${PORT}`);
});
