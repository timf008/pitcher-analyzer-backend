const express = require("express");
const path = require("path");
const { exec } = require("child_process");
const csv = require("csv-parser");
const app = express();
const fs = require("fs");


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

// --------------------------------------
// API: Return list of pitchers for season
// --------------------------------------
app.get("/api/pitcherList", (req, res) => {
    const { season } = req.query;

    const filePath = path.join(__dirname, `stathead_pitching_${season}.csv`);

    const rows = [];
    fs.createReadStream(filePath)
        .pipe(csv())
        .on("data", (row) => {

            // Extract fields safely
            const name = row.Player;
            const id = row["Player-additional"] || null;

            // Convert GS to number
            const gs = Number(row.GS || row.gs || 0);

            // ⭐ FILTER: Only include pitchers with at least 10 starts
            if (name && gs >= 10) {
                rows.push({ name, id });
            }
        })
        .on("end", () => {
            res.json(rows);
        });
});


// ---------------------------
// Backend for Emoji Trend Button - Graph
// ---------------------------
app.get("/api/pitcherTrend", (req, res) => {
    const { name, stat } = req.query;

    const seasons = [2025, 2024, 2023, 2022, 2021, 2020];

    const results = [];
    let completed = 0;

    seasons.forEach(season => {
        const cmd = `Rscript "${path.join(__dirname, "trend.r")}" "${name}" "${stat}" ${season}`;

        exec(cmd, (error, stdout, stderr) => {
            completed++;

            console.log(`RAW TREND OUTPUT for ${season}:`, stdout);

            if (error) {
                console.error("R error:", stderr);
                results.push({ season, value: null });
            } else {
                try {
                    const json = JSON.parse(stdout);
                    let value = json.value;

                    // ⭐ THE FIX — convert {} → null
                    if (value && typeof value === "object" && Object.keys(value).length === 0) {
                        value = null;
                    }

                    results.push({ season, value });

                } catch (e) {
                    console.error("Trend parse error:", e);
                    results.push({ season, value: null });
                }
            }

            if (completed === seasons.length) {
                results.sort((a, b) => a.season - b.season);
                console.log("TREND FINAL RESULTS:", results);
                res.json(results);
            }
        });
    });
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
