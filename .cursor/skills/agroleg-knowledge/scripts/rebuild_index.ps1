param(
    [Parameter(Mandatory = $false)]
    [string]$ProjetosPath = "Cobol/Projetos",
    [Parameter(Mandatory = $false)]
    [string]$OutputPath = ".cursor/skills/agroleg-knowledge/index-generated.md"
)

$ErrorActionPreference = "Stop"

$root = $ProjetosPath
if (-not (Test-Path -LiteralPath $root)) {
    throw "ProjetosPath nao encontrado: $root"
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

function Get-SectionContent {
    param(
        [string]$Text,
        [string[]]$Headers
    )
    foreach ($h in $Headers) {
        $escaped = [regex]::Escape($h)
        $m = [regex]::Match($Text, "(?ms)^##\s+$escaped\s*\r?\n(.*?)(?=^##\s+|\z)")
        if ($m.Success) {
            return $m.Groups[1].Value.Trim()
        }
    }
    return ""
}

function Get-ChaveFromReadme {
    param([string]$Text, [string]$FolderName)
    $jira = Get-SectionContent -Text $Text -Headers @("Dados Jira (sync)")
    if ($jira -match "(?m)^-\s*Chave:\s*(AGROLEG-\d+)") {
        return $Matches[1]
    }
    if ($FolderName -match "AGROLEG-\d+") {
        return $Matches[0]
    }
    return $FolderName
}

function Extract-Modules {
    param([string]$Text)
    $mods = New-Object "System.Collections.Generic.HashSet[string]"
    $cc = Get-SectionContent -Text $Text -Headers @("Centros de custo impactados")
    if ($cc) {
        [regex]::Matches($cc, "\b(RUCA|RUEC|RCOR|CRUR|CMRU|RURA|RUFI|RUMO|RURC)\b") | ForEach-Object {
            [void]$mods.Add($_.Value.ToUpperInvariant())
        }
    }
    [regex]::Matches($Text, "\b(RUCA|RUEC|RCOR|CRUR|CMRU|RUFI|RUMO|RURC)\d{3,4}\b", [System.Text.RegularExpressions.RegexOptions]::IgnoreCase) | ForEach-Object {
        $prefix = $_.Value.Substring(0, 4).ToUpperInvariant()
        [void]$mods.Add($prefix)
    }
    return @($mods | Sort-Object)
}

function Extract-SicorMessages {
    param([string]$Text)
    $msgs = New-Object "System.Collections.Generic.HashSet[string]"
    $combined = @(
        (Get-SectionContent -Text $Text -Headers @("Requisito de negocio (SICOR)")),
        (Get-SectionContent -Text $Text -Headers @("Objetivo"))
    ) -join "`n"
    [regex]::Matches($combined, "\b(COR|CIR)\d{4}[A-Z0-9]*\b", [System.Text.RegularExpressions.RegexOptions]::IgnoreCase) | ForEach-Object {
        [void]$msgs.Add($_.Value.ToUpperInvariant())
    }
    if ($combined -match "5\.12|SICOR\s*512|SICOR 512") {
        [void]$msgs.Add("SICOR-5.12")
    }
    return @($msgs | Sort-Object)
}

function Extract-Status {
    param([string]$Text)
    $statusSec = Get-SectionContent -Text $Text -Headers @("Status", "Dados Jira (sync)")
    if ($Text -match "(?m)^-\s*\[x\]\s*Implantado") { return "Finalizada" }
    if ($Text -match "(?m)^-\s*\[x\]\s*Pronto para subida") { return "Pronto para subida" }
    if ($statusSec -match "(?m)^-\s*Status:\s*(.+)$") { return $Matches[1].Trim() }
    if ($Text -match "Finalizada") { return "Finalizada" }
    return "Em andamento"
}

$readmes = Get-ChildItem -LiteralPath $root -Recurse -Filter README.md -File |
    Where-Object { $_.FullName -match "AGROLEG-\d+" }

$stories = @()
foreach ($readme in $readmes) {
    $text = Read-TextFileUtf8 -Path $readme.FullName
    if ([string]::IsNullOrWhiteSpace($text)) { continue }
    $folder = Split-Path (Split-Path $readme.FullName -Parent) -Leaf
    if ($folder -notmatch "AGROLEG-\d+") {
        $folder = Split-Path $readme.FullName -Parent | Split-Path -Leaf
    }
    $chave = Get-ChaveFromReadme -Text $text -FolderName $folder
    $relPath = $readme.FullName -replace [regex]::Escape((Get-Location).Path + "\"), "" -replace "\\", "/"
    $stories += [PSCustomObject]@{
        Chave    = $chave
        Status   = Extract-Status -Text $text
        Modulos  = (Extract-Modules -Text $text) -join ", "
        Sicor    = (Extract-SicorMessages -Text $text) -join ", "
        Caminho  = $relPath
    }
}

$stories = $stories | Sort-Object Chave -Unique

$byMsg = @{}
$byMod = @{}
foreach ($s in $stories) {
    if ($s.Sicor) {
        foreach ($m in ($s.Sicor -split ",\s*")) {
            if (-not $byMsg.ContainsKey($m)) { $byMsg[$m] = @() }
            $byMsg[$m] += $s.Chave
        }
    }
    if ($s.Modulos) {
        foreach ($mod in ($s.Modulos -split ",\s*")) {
            if (-not $byMod.ContainsKey($mod)) { $byMod[$mod] = @() }
            $byMod[$mod] += $s.Chave
        }
    }
}

$lines = New-Object System.Collections.Generic.List[string]
$lines.Add("# Indice AGROLEG (gerado automaticamente)")
$lines.Add("")
$lines.Add("> Gerado em: $(Get-Date -Format 'yyyy-MM-dd HH:mm')")
$lines.Add("> Fonte: READMEs em ``$root``")
$lines.Add("> Revisar e mesclar em ``index.md`` se necessario.")
$lines.Add("")
$lines.Add("## Por mensagem SICOR")
$lines.Add("")
$lines.Add("| Mensagem | Historias |")
$lines.Add("|---|---|")
if ($byMsg.Count -eq 0) {
    $lines.Add("| (nenhuma detectada) | |")
} else {
    foreach ($k in ($byMsg.Keys | Sort-Object)) {
        $hist = ($byMsg[$k] | Sort-Object -Unique) -join ", "
        $lines.Add("| $k | $hist |")
    }
}
$lines.Add("")
$lines.Add("## Por modulo")
$lines.Add("")
$lines.Add("| Modulo | Historias |")
$lines.Add("|---|---|")
if ($byMod.Count -eq 0) {
    $lines.Add("| (nenhum detectado) | |")
} else {
    foreach ($k in ($byMod.Keys | Sort-Object)) {
        $hist = ($byMod[$k] | Sort-Object -Unique) -join ", "
        $lines.Add("| $k | $hist |")
    }
}
$lines.Add("")
$lines.Add("## Historias indexadas")
$lines.Add("")
$lines.Add("| Historia | Status | Modulos | SICOR | Caminho README |")
$lines.Add("|---|---|---|---|---|")
foreach ($s in $stories) {
    $sicor = if ($s.Sicor) { $s.Sicor } else { "N/A" }
    $mods = if ($s.Modulos) { $s.Modulos } else { "-" }
    $lines.Add("| $($s.Chave) | $($s.Status) | $mods | $sicor | $($s.Caminho) |")
}

$content = $lines -join [Environment]::NewLine
$enc = New-Object System.Text.UTF8Encoding $false
[System.IO.File]::WriteAllText($OutputPath, $content, $enc)
Write-Output $content
