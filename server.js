const express = require("express");
const cors = require("cors");
const path = require("path");
const { exec } = require("child_process");
const fs = require("fs");

const app = express();

// ---------------------------
// GLOBAL CORS (must be first)
// ---------------------------
app.use(cors({
    origin: "*",
    methods: ["GET", "POST", "OPTIONS"],
    allowedHeaders: ["Content-Type"]
}));

// Remove your manual header middleware — not needed
// app.use((req, res, next) => {
//     res.setHeader("Access-Control-Allow-Origin", "*");
//     next();
// });


// ---------------------------
// Name Normalization (Latin accents → ASCII)
// ---------------------------
function normalizeNameBackend(x) {
    return x
        .normalize("NFKD")               // split accents
        .replace(/[\u0300-\u036f]/g, "") // remove diacritics
        .replace(/[^\w\s-]/g, "")        // remove non-ASCII
        .replace(/\s+/g, " ")            // collapse spaces
        .trim()
        .toUpperCase();                  // match CSV uppercase
}

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

// ===========================================================
// API ROUTES — MUST COME BEFORE STATIC FILES
// ===========================================================

// ---------------------------
// API: Run R script for pitcher data
// ---------------------------
app.get("/api/pitchers", async (req, res) => {
    let { name, season } = req.query;

    if (!name || !season) {
        return res.status(400).json({ error: "Missing name or season" });
    }

    // ⭐ Normalize accented names
    name = normalizeNameBackend(name);

    // ⭐ DEBUG LOG — THIS IS WHAT WE NEED
    console.log("Normalized name sent to R:", name);

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
// Rscript wrapper for leaders / compare / trend
// ---------------------------
const { spawn } = require("child_process");

function runRScript(scriptName, args = []) {
    return new Promise((resolve, reject) => {
        const child = spawn("Rscript", [scriptName, ...args], {
            cwd: __dirname
        });

        let output = "";
        let errorOutput = "";

        child.stdout.on("data", (data) => {
            output += data.toString();
        });

        child.stderr.on("data", (data) => {
            errorOutput += data.toString();
        });

        child.on("close", (code) => {
            if (code !== 0) {
                return reject(new Error(errorOutput));
            }
            resolve(output);
        });
    });
}


// --------------------------------------
// API: Pitching Leaders Button
// --------------------------------------
app.get("/api/pitching/leaders", async (req, res) => {
    try {
        const season = req.query.season;

        if (!season) {
            return res.status(400).json({ error: "Season required" });
        }

        // leaders.r is your pitching version
        const result = await runRScript("leaders.r", [season]);
        const data = JSON.parse(result);

        res.json(data);

    } catch (err) {
        console.error("Pitching Leaders API error:", err);
        res.status(500).json({ error: "Server error" });
    }
});


// --------------------------------------
// API: League Averages (XP + Overall)
// --------------------------------------
app.get("/api/pitching/averages", async (req, res) => {
    try {
        const season = req.query.season;
        if (!season) return res.status(400).json({ error: "Season required" });

        const result = await runRScript("averages.r", [season]);
        const data = JSON.parse(result);

        res.json(data);
    } catch (err) {
        console.error("Pitching averages API error:", err);
        res.status(500).json({ error: "Server error" });
    }
});

// --------------------------------------
// API: Player of the Day
// --------------------------------------
app.get("/api/player-of-day", async (req, res) => {
    try {
        const season = req.query.season;
        if (!season) return res.status(400).json({ error: "Season required" });

        const result = await runRScript("player_of_day.r", [season]);
        const data = JSON.parse(result);

        res.json(data);
    } catch (err) {
        console.error("Player of Day API error:", err);
        res.status(500).json({ error: "Server error" });
    }
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

// ===========================================================
// STATIC FILES — MUST COME LAST
// ===========================================================
app.use(express.static(path.join(__dirname, "public")));

// ---------------------------
// Start Server
// ---------------------------
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`Pitcher Analyzer running at http://localhost:${PORT}`);
});



