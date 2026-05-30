param(
    [Parameter(Mandatory = $true)]
    [string]$DemandaPath,
    [Parameter(Mandatory = $true)]
    [string]$LibsPath,
    [Parameter(Mandatory = $false)]
    [string]$ReadmePath,
    [switch]$InPlace
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path -LiteralPath $DemandaPath)) {
    throw "DemandaPath nao encontrado: $DemandaPath"
}
if (-not (Test-Path -LiteralPath $LibsPath)) {
    throw "LibsPath nao encontrado: $LibsPath"
}
if ($InPlace -and [string]::IsNullOrWhiteSpace($ReadmePath)) {
    throw "ReadmePath e obrigatorio com -InPlace"
}
if ($InPlace -and -not (Test-Path -LiteralPath $ReadmePath)) {
    throw "ReadmePath nao encontrado: $ReadmePath"
}

function Read-TextFileUtf8 {
    param([string]$Path)
    $bytes = [System.IO.File]::ReadAllBytes($Path)
    if ($bytes.Length -eq 0) { return "" }
    if ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
        return [System.Text.Encoding]::UTF8.GetString($bytes, 3, $bytes.Length - 3)
    }
    $encUtf8 = New-Object System.Text.UTF8Encoding $false, $false
    return $encUtf8.GetString($bytes)
}

function Write-TextFileUtf8NoBom {
    param(
        [string]$Path,
        [string]$Content
    )
    $enc = New-Object System.Text.UTF8Encoding $false
    [System.IO.File]::WriteAllText($Path, $Content, $enc)
}

$libsRoot = (Resolve-Path -LiteralPath $LibsPath).Path

$libsIndex = @{}
Get-ChildItem -LiteralPath $libsRoot -Recurse -File -Filter *.cpy | ForEach-Object {
    $key = $_.BaseName.ToUpperInvariant()
    if (-not $libsIndex.ContainsKey($key)) {
        $libsIndex[$key] = @()
    }
    $libsIndex[$key] += $_.FullName
}

$depRegexCopy = [regex]'(?i)\bCOPY\s+["'']?([A-Z0-9#_-]+)["'']?\s*\.'
$inclNameRegex = [regex]'(?i)\bINCLUDE\s+([A-Z0-9#_-]+)\b'

$deps = New-Object "System.Collections.Generic.HashSet[string]"
$depType = @{}

$allowedExt = @(".cbl", ".cpy", ".mfs", ".sql", ".jcl")
Get-ChildItem -LiteralPath $DemandaPath -Recurse -File | Where-Object { $allowedExt -contains $_.Extension.ToLowerInvariant() } | ForEach-Object {
    $content = Get-Content -LiteralPath $_.FullName
    $insideExecSql = $false
    foreach ($line in $content) {
        $trim = $line.Trim()
        if ($trim.StartsWith("*")) {
            continue
        }

        $copyMatch = $depRegexCopy.Match($line)
        if ($copyMatch.Success) {
            $name = $copyMatch.Groups[1].Value.ToUpperInvariant()
            if ($deps.Add($name)) { $depType[$name] = "COPYBOOK" }
            continue
        }

        if ($line -match '(?i)\bEXEC\s+SQL\b') {
            $insideExecSql = $true
        }
        if ($insideExecSql) {
            $incMatch = $inclNameRegex.Match($line)
            if ($incMatch.Success) {
                $name = $incMatch.Groups[1].Value.ToUpperInvariant()
                if ($deps.Add($name)) { $depType[$name] = "DCLGEN" }
            }
            if ($line -match '(?i)\bEND-EXEC\b') {
                $insideExecSql = $false
            }
        }
    }
}

$foundCopy = @()
$foundDcl = @()
$missing = @()
$system = @()

$demandaRoot = (Resolve-Path -LiteralPath $DemandaPath).Path

$deps | Sort-Object | ForEach-Object {
    $dep = $_
    if ($libsIndex.ContainsKey($dep)) {
        $paths = $libsIndex[$dep] | ForEach-Object {
            ($_ -replace [regex]::Escape($libsRoot), "Cobol/LIBS") -replace "\\", "/"
        }
        $item = [PSCustomObject]@{
            Name = $dep
            Paths = $paths
        }
        if ($depType[$dep] -eq "DCLGEN") {
            $foundDcl += $item
        } else {
            $foundCopy += $item
        }
    } else {
        $localCpy = Get-ChildItem -LiteralPath $demandaRoot -Recurse -File -Filter "*.cpy" -ErrorAction SilentlyContinue |
            Where-Object { $_.BaseName.ToUpperInvariant() -eq $dep }
        if ($localCpy) {
            $paths = $localCpy | ForEach-Object {
                $_.FullName -replace [regex]::Escape((Get-Location).Path + "\"), "" -replace "\\", "/"
            }
            $item = [PSCustomObject]@{
                Name = $dep
                Paths = $paths
            }
            if ($depType[$dep] -eq "DCLGEN") {
                $foundDcl += $item
            } else {
                $foundCopy += $item
            }
        } elseif ($dep -eq "SQLCA") {
            $system += $dep
        } else {
            $missing += $dep
        }
    }
}

$out = New-Object System.Collections.Generic.List[string]
$out.Add("## Dependencias LIBS")
$out.Add("- COPYBOOKS (encontradas):")
if ($foundCopy.Count -eq 0) {
    $out.Add("  - Nenhuma")
} else {
    foreach ($item in ($foundCopy | Sort-Object Name)) {
        foreach ($p in $item.Paths) {
            $out.Add("  - $($item.Name) -> $p")
        }
    }
}
$out.Add("- DCLGEN (encontradas):")
if ($foundDcl.Count -eq 0) {
    $out.Add("  - Nenhuma")
} else {
    foreach ($item in ($foundDcl | Sort-Object Name)) {
        foreach ($p in $item.Paths) {
            $out.Add("  - $($item.Name) -> $p")
        }
    }
}
$out.Add("- Pendencias (nao encontradas em LIBS):")
if ($missing.Count -eq 0) {
    $out.Add("  - Nenhuma")
} else {
    foreach ($m in ($missing | Sort-Object -Unique)) {
        $out.Add("  - $m")
    }
}
$out.Add("- Dependencias de sistema:")
if ($system.Count -eq 0) {
    $out.Add("  - Nenhuma")
} else {
    foreach ($s in ($system | Sort-Object -Unique)) {
        $out.Add("  - $s")
    }
}

$blockText = $out -join [Environment]::NewLine

if (-not $InPlace) {
    Write-Output $blockText
    exit 0
}

$readme = Read-TextFileUtf8 -Path $ReadmePath
$sectionPattern = "(?ms)^## Dependencias LIBS\s*\r?\n.*?(?=^##\s+|\z)"
$altPattern = "(?ms)^## Dependencias \(LIBS\)\s*\r?\n.*?(?=^##\s+|\z)"

if ($readme -match $sectionPattern) {
    $readme = [regex]::Replace(
        $readme,
        $sectionPattern,
        $blockText.TrimEnd() + [Environment]::NewLine + [Environment]::NewLine,
        1
    )
} elseif ($readme -match $altPattern) {
    $readme = [regex]::Replace(
        $readme,
        $altPattern,
        $blockText.TrimEnd() + [Environment]::NewLine + [Environment]::NewLine,
        1
    )
} else {
    if ($readme -match "(?m)^## Riscos e atencoes\b") {
        $readme = [regex]::Replace(
            $readme,
            "(?m)^## Riscos e atencoes\b",
            $blockText + [Environment]::NewLine + [Environment]::NewLine + "## Riscos e atencoes",
            1
        )
    } elseif ($readme -match "(?m)^## Dados Jira \(sync\)") {
        $readme = [regex]::Replace(
            $readme,
            "(?m)^## Dados Jira \(sync\)",
            $blockText + [Environment]::NewLine + [Environment]::NewLine + "## Dados Jira (sync)",
            1
        )
    } else {
        $readme = $readme.TrimEnd() + [Environment]::NewLine + [Environment]::NewLine + $blockText + [Environment]::NewLine
    }
}

Write-TextFileUtf8NoBom -Path $ReadmePath -Content $readme
Write-Output $blockText
