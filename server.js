const express = require("express");
const cors = require("cors");   // MUST be first import
const path = require("path");
const { exec } = require("child_process");
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

    // ⭐ FIX: FORCE R TO RUN IN THE CORRECT DIRECTORY
    const cmd = `cd "${__dirname}" && Rscript "stathead.r" "${name}" "${season}"`;

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

// ---------------------------
// API: Trend data (current + previous season)
// ---------------------------
app.get("/api/pitcherTrend", async (req, res) => {
    const { name, stat, season } = req.query;

    if (!name || !stat || !season) {
        return res.status(400).json({ error: "Missing name, stat, or season" });
    }

    const currentSeason = Number(season);
    const previousSeason = currentSeason - 1;

    const results = {};

    async function runTrend(seasonNumber) {
        const cmd = `cd "${__dirname}" && Rscript "trend.r" "${name}" "${stat}" ${seasonNumber}`;
        const output = await runR(cmd);

        if (!output) return null;

        try {
            const json = JSON.parse(output);
            return (json && "value" in json) ? json.value : null;
        } catch (e) {
            console.error("Trend JSON parse error:", e);
            console.log("Raw R output:", output);
            return null;
        }
    }

    const currentValue = await runTrend(currentSeason);
    const previousValue = await runTrend(previousSeason);

    results.currentSeason = currentSeason;
    results.currentValue = currentValue;

    results.previousSeason = previousSeason;
    results.previousValue = previousValue;

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
// Debug
// ---------------------------
app.get("/api/debug/stathead", async (req, res) => {
    const filePath = path.join(__dirname, "stathead.r");
    const exists = fs.existsSync(filePath);
    res.json({ stathead_exists: exists, path: filePath });
});

app.get("/api/debug/rscript", async (req, res) => {
    exec("which Rscript", (err, stdout) => {
        res.json({
            rscript_path: stdout.trim(),
            error: err ? err.message : null
        });
    });
});

app.get("/api/debug/csv", async (req, res) => {
    const season = req.query.season || "2026";
    const filePath = path.join(__dirname, `stathead_pitching_${season}.csv`);
    const exists = fs.existsSync(filePath);
    res.json({ csv_exists: exists, path: filePath });
});

app.get("/api/debug/r-read", async (req, res) => {
    const cmd = `cd "${__dirname}" && Rscript -e "library(readr); df <- read_csv('stathead_pitching_2026.csv', show_col_types=FALSE); print(head(df))"`;
    const output = await runR(cmd);
    res.send(`<pre>${output || "NO OUTPUT"}</pre>`);
});

// ---------------------------
// Start Server
// ---------------------------
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`Pitcher Analyzer running at http://localhost:${PORT}`);
});

