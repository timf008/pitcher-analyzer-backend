const express = require("express");
const cors = require("cors");   // MUST be first import
const path = require("path");
const { exec } = require("child_process");
const csv = require("csv-parser");
const app = express();
const fs = require("fs");

// ---------------------------
// GLOBAL CORS (MUST BE FIRST MIDDLEWARE)
// ---------------------------
app.use(cors());

// Force CORS on ALL responses (including errors)
app.use((req, res, next) => {
    res.setHeader("Access-Control-Allow-Origin", "*");
    next();
});

// ---------------------------
// Static File Serving
// ---------------------------
app.use(express.static(path.join(__dirname, "public")));

// ---------------------------
// Safe Rscript wrapper with timeout
// ---------------------------
function runR(cmd, timeoutMs = 8000) {
    return new Promise((resolve) => {
        exec(cmd, { timeout: timeoutMs }, (error, stdout) => {
            if (error) {
                console.error("R crashed or timed out:", error);
                return resolve(null);
            }
            resolve(stdout);
        });
    });
}

// ---------------------------
// API: Run R script for pitcher data
// ---------------------------
app.get("/api/pitchers", async (req, res) => {
    const { name, season } = req.query;

    if (!name || !season) {
        return res.status(400).json({ error: "Missing name or season" });
    }

    const cmd = `Rscript "${path.join(__dirname, "stathead.r")}" "${name}" "${season}"`;

    const output = await runR(cmd);

    if (!output) {
        return res.status(500).json({ error: "R timeout or crash" });
    }

    try {
        const json = JSON.parse(output);
        return res.json(json);
    } catch (e) {
        console.error("JSON parse error:", e);
        console.log("Raw R output:", output);
        return res.status(500).json({ error: "Invalid JSON from R" });
    }
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

        const output = await runR(cmd);

        if (!output) {
            results.push({ season, value: null });
            continue;
        }

        try {
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
