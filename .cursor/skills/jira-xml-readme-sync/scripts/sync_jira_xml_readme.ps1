param(
    [Parameter(Mandatory = $true)]
    [string]$XmlPath,
    [Parameter(Mandatory = $true)]
    [string]$ReadmePath,
    [switch]$InPlace
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path -LiteralPath $XmlPath)) {
    throw "XmlPath nao encontrado: $XmlPath"
}
if (-not (Test-Path -LiteralPath $ReadmePath)) {
    throw "ReadmePath nao encontrado: $ReadmePath"
}

function Read-TextFileUtf8 {
    param([string]$Path)
    $bytes = [System.IO.File]::ReadAllBytes($Path)
    if ($bytes.Length -eq 0) { return "" }
    # UTF-8 BOM
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

# Quando UTF-8 foi lido como Latin-1 (Get-Content sem Encoding), aparecem sequencias tipo "Ã§", "Ã£", "Ã¡".
# Nao aplicar em qualquer string com U+00C3 (ex.: resumo Jira com "ÇÃ" legitimo no XML exportado).
function Repair-MojibakeUtf8 {
    param([string]$Text)
    if ([string]::IsNullOrWhiteSpace($Text)) { return $Text }
    if ($Text -notmatch "\u00C3[\u00A7\u00A3\u00A1\u00A9\u00AD\u00AA\u00B4]") { return $Text }
    try {
        $latin1 = [System.Text.Encoding]::GetEncoding("ISO-8859-1")
        $bytes = $latin1.GetBytes($Text)
        return [System.Text.Encoding]::UTF8.GetString($bytes)
    } catch {
        return $Text
    }
}

$xmlRaw = Read-TextFileUtf8 -Path $XmlPath

# Alguns exports salvos pelo navegador incluem texto antes do XML.
# Remove qualquer prefixo antes de <?xml ou <rss para permitir parse.
$startXml = $xmlRaw.IndexOf("<?xml")
$startRss = $xmlRaw.IndexOf("<rss")
$startPos = -1
if ($startXml -ge 0 -and $startRss -ge 0) {
    $startPos = [Math]::Min($startXml, $startRss)
} elseif ($startXml -ge 0) {
    $startPos = $startXml
} elseif ($startRss -ge 0) {
    $startPos = $startRss
}
if ($startPos -gt 0) {
    $xmlRaw = $xmlRaw.Substring($startPos)
}

# Jira pode trazer entidades HTML nao declaradas em XML (ex.: &nbsp;).
# Normaliza entidades nomeadas desconhecidas para espaco.
$xmlRaw = [regex]::Replace(
    $xmlRaw,
    "&(?!amp;|lt;|gt;|apos;|quot;|#[0-9]+;|#x[0-9A-Fa-f]+;)[A-Za-z][A-Za-z0-9]+;",
    " "
)

# Jira RSS export: evitar XmlDocument (pode corromper texto UTF-8 em <summary>/<status>).
function Get-TagInnerFromBlock {
    param(
        [string]$Block,
        [string]$Tag,
        [switch]$AllowEmpty
    )
    if ([string]::IsNullOrEmpty($Block)) { return $null }
    $esc = [regex]::Escape($Tag)
    $m = [regex]::Match($Block, "<$esc(?:\s[^>]*)?>([^<]*)</$esc>", [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
    if (-not $m.Success) { return $null }
    $val = $m.Groups[1].Value.Trim()
    if (-not $AllowEmpty -and [string]::IsNullOrWhiteSpace($val)) { return $null }
    if ($AllowEmpty) { return $val }
    return $val
}

# Jira RSS costuma exportar resumo com sequencia UTF-8 errada: C3 87 C3 83 (Ç U+00C7 + Ã U+00C3) + O em vez de c3 a7 c3 a3 6f (ção).
function Normalize-JiraSummaryTypo {
        param([string]$Text)
        if ([string]::IsNullOrWhiteSpace($Text)) { return $Text }
        $pat = [string]::Concat([char]0x00C7, [char]0x00C3, "([Oo])")
        return [regex]::Replace($Text, $pat, {
            param($m)
            $o = $m.Groups[1].Value.ToLowerInvariant()
            return [string]::Concat([char]0x00E7, [char]0x00E3, $o)
        })
}

# <description> no RSS Jira vem com HTML; extrai trecho de negocio ate "Detalhamento do negocio" e converte para texto (markdown simples com bullets).
function Get-ObjectiveFromItemDescription {
    param([string]$ItemXml)
    if ([string]::IsNullOrEmpty($ItemXml)) { return $null }
    $dm = [regex]::Match($ItemXml, '(?is)<description(?:\s[^>]*)?>(.*?)</description>')
    if (-not $dm.Success) { return $null }
    $html = $dm.Groups[1].Value.Trim()
    if ([string]::IsNullOrWhiteSpace($html)) { return $null }

    # Corta antes de secoes longas (objetivo = historia de usuario + escopo resumido).
    $cutRx = '(?is)<p>\s*<b>\s*Detalhamento do neg'
    $cut = [regex]::Match($html, $cutRx)
    if ($cut.Success) {
        $html = $html.Substring(0, $cut.Index).Trim()
    }

    # Decodifica entidades comuns (alem do preprocessamento de &nbsp; no XML bruto).
    $t = $html
    $t = $t -replace '(?i)&nbsp;', ' '
    $t = $t -replace '(?i)&amp;', '&'
    $t = $t -replace '(?i)&lt;', '<'
    $t = $t -replace '(?i)&gt;', '>'
    $t = $t -replace '(?i)&quot;', '"'
    $t = [regex]::Replace($t, '&#(\d+);', {
        param($m)
        try {
            return [string][char][int]$m.Groups[1].Value
        } catch {
            return $m.Value
        }
    })
    $t = [regex]::Replace($t, '(?i)&#x([0-9A-Fa-f]+);', {
        param($m)
        try {
            return [string][char][Convert]::ToInt32($m.Groups[1].Value, 16)
        } catch {
            return $m.Value
        }
    })

    # Listas e quebras viram linhas.
    $t = [regex]::Replace($t, '(?i)<br\s*/?>', "`n")
    $t = [regex]::Replace($t, '(?i)</p>', "`n")
    $t = [regex]::Replace($t, '(?i)</li>', "`n")
    $t = [regex]::Replace($t, '(?i)<li[^>]*>', "- ")

    # Remove demais tags.
    $t = [regex]::Replace($t, '(?s)<[^>]+>', ' ')

    # Normaliza espacos e linhas.
    $t = [regex]::Replace($t, '[ \t]{2,}', ' ')
    $t = [regex]::Replace($t, '(\r?\n){2,}', "`n")
    $lines = @()
    foreach ($line in ($t -split "`n")) {
        $ln = $line.Trim()
        if ($ln -eq '') { continue }
        # Normaliza item que ja comeca com hifen colado ("-Corrigir" -> "Corrigir")
        if ($ln.StartsWith('-') -and -not $ln.StartsWith('- ')) {
            $ln = $ln.TrimStart('-').Trim()
        }
        if (-not $ln.StartsWith('- ')) {
            $ln = "- $ln"
        }
        $core = $ln.Substring(2).Trim()
        if ($core -match '^(Identificação|Identificacao)$') { continue }
        if ($core -eq 'Quero') { continue }
        if ($core -eq 'Para') { continue }
        while ($ln.StartsWith('- -')) {
            $ln = '- ' + $ln.Substring(2).TrimStart()
        }
        $lines += $ln
    }
    if ($lines.Count -eq 0) { return $null }
    $out = ($lines -join [Environment]::NewLine)
    $out = Normalize-JiraSummaryTypo -Text $out
    $out = Repair-MojibakeUtf8 -Text $out
    return $out
}

$itemMatch = [regex]::Match($xmlRaw, "(?s)<item\s*>(.*?)</item\s*>")
$itemXml = $null
if ($itemMatch.Success) {
    $itemXml = $itemMatch.Groups[1].Value
}

$key = $null
$title = $null
$status = $null
$assignee = $null
$updated = $null

if (-not [string]::IsNullOrEmpty($itemXml)) {
    $key = Get-TagInnerFromBlock -Block $itemXml -Tag "key"
    $title = Get-TagInnerFromBlock -Block $itemXml -Tag "summary"
    $status = Get-TagInnerFromBlock -Block $itemXml -Tag "status"
    $assignee = Get-TagInnerFromBlock -Block $itemXml -Tag "assignee"
    $updated = Get-TagInnerFromBlock -Block $itemXml -Tag "updated"
    if ([string]::IsNullOrWhiteSpace($updated)) {
        $updated = Get-TagInnerFromBlock -Block $itemXml -Tag "updatedDate"
    }
}

if ([string]::IsNullOrWhiteSpace($key)) {
    $km = [regex]::Match($xmlRaw, "<key(?:\s[^>]*)?>([^<]+)</key>", [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
    if ($km.Success) { $key = $km.Groups[1].Value.Trim() }
}
if ([string]::IsNullOrWhiteSpace($title)) {
    $title = Get-TagInnerFromBlock -Block $xmlRaw -Tag "summary"
}
if ([string]::IsNullOrWhiteSpace($status)) {
    $status = Get-TagInnerFromBlock -Block $xmlRaw -Tag "status"
}
if ([string]::IsNullOrWhiteSpace($assignee)) {
    $assignee = Get-TagInnerFromBlock -Block $xmlRaw -Tag "assignee"
}
if ([string]::IsNullOrWhiteSpace($updated)) {
    $updated = Get-TagInnerFromBlock -Block $xmlRaw -Tag "updated"
}
if ([string]::IsNullOrWhiteSpace($updated)) {
    $updated = Get-TagInnerFromBlock -Block $xmlRaw -Tag "updatedDate"
}

if ([string]::IsNullOrWhiteSpace($key)) { $key = "Nao informado" }
if ([string]::IsNullOrWhiteSpace($title)) { $title = "Nao informado" }
if ([string]::IsNullOrWhiteSpace($status)) { $status = "Nao informado" }
if ([string]::IsNullOrWhiteSpace($assignee)) { $assignee = "Nao informado" }
if ([string]::IsNullOrWhiteSpace($updated)) { $updated = "Nao informado" }

$title = Normalize-JiraSummaryTypo -Text $title
$title = Repair-MojibakeUtf8 -Text $title
$status = Repair-MojibakeUtf8 -Text $status
$assignee = Repair-MojibakeUtf8 -Text $assignee

$objetivo = Get-ObjectiveFromItemDescription -ItemXml $itemXml
if ([string]::IsNullOrWhiteSpace($objetivo)) {
    $objetivo = "Nao foi possivel extrair objetivo do XML (item/description vazio ou formato nao reconhecido)."
}

$blockLines = @(
    "## Dados Jira (sync)",
    "- Chave: $key",
    "- Titulo: $title",
    "- Status: $status",
    "- Responsavel: $assignee",
    "- Atualizado em: $updated"
)
$blockText = ($blockLines -join [Environment]::NewLine)

if (-not $InPlace) {
    Write-Output ($blockText + [Environment]::NewLine + [Environment]::NewLine + "## Objetivo (preview)" + [Environment]::NewLine + $objetivo)
    exit 0
}

$readme = Read-TextFileUtf8 -Path $ReadmePath

# Atualiza secao Objetivo (conteudo vindo do campo description do Jira).
if ($readme -match "(?ms)^## Objetivo\s*\r?\n") {
    $objPlain = $objetivo
    $readme = [regex]::Replace(
        $readme,
        "(?ms)^## Objetivo\s*\r?\n.*?(?=^##\s+|\z)",
        [System.Text.RegularExpressions.MatchEvaluator]{
            param($m)
            "## Objetivo" + [Environment]::NewLine + $objPlain + [Environment]::NewLine + [Environment]::NewLine
        },
        1
    )
}

$startHeader = "## Dados Jira (sync)"
$nextHeaderPattern = "(?m)^##\s+"

if ($readme -match "(?ms)^## Dados Jira \(sync\).*?(?=^##\s+|\z)") {
    $readme = [regex]::Replace(
        $readme,
        "(?ms)^## Dados Jira \(sync\).*?(?=^##\s+|\z)",
        [System.Text.RegularExpressions.MatchEvaluator]{
            param($m) $blockText.TrimEnd() + [Environment]::NewLine + [Environment]::NewLine
        },
        1
    )
} else {
    if ($readme -match "(?m)^## Status\b") {
        $readme = [regex]::Replace(
            $readme,
            "(?m)^## Status\b",
            $blockText + [Environment]::NewLine + [Environment]::NewLine + "## Status",
            1
        )
    } else {
        $readme = $readme.TrimEnd() + [Environment]::NewLine + [Environment]::NewLine + $blockText + [Environment]::NewLine
    }
}

Write-TextFileUtf8NoBom -Path $ReadmePath -Content $readme
$blockText
