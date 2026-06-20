#Requires -Version 5.1
<#
.SYNOPSIS
  Migra copybooks .cpy de Cobol/Projetos/Stefanini para Cobol/LIBS (COPYLIB/DCLGEN).
  Politica de duplicata: manter LIBS; apagar Stefanini se identico; conflito se diferente.
#>
param(
    [switch]$WhatIf
)

$ErrorActionPreference = 'Stop'
$RepoRoot = Resolve-Path (Join-Path $PSScriptRoot '..\..\..\..')
$SourceRoot = Join-Path $RepoRoot 'Cobol\Projetos\Stefanini'
$LibsRoot = Join-Path $RepoRoot 'Cobol\LIBS'
$ReportPath = Join-Path $PSScriptRoot 'migrate-stef-copy-conflicts.md'

function Get-DestinationPath {
    param([string]$FileName)

    $base = [System.IO.Path]::GetFileNameWithoutExtension($FileName).ToLowerInvariant()
    $ext = [System.IO.Path]::GetExtension($FileName).ToLowerInvariant()
    $member = "$base$ext"

    if ($base -match '^gfctb') {
        return Join-Path (Join-Path $LibsRoot 'DCLGEN\GFCT') $member
    }
    if ($base -match '^cliev') {
        return Join-Path (Join-Path $LibsRoot 'DCLGEN\CLIE') $member
    }
    if ($base -match '^objev') {
        return Join-Path (Join-Path $LibsRoot 'DCLGEN\OBJE') $member
    }
    return Join-Path (Join-Path $LibsRoot 'COPYLIB\GFCT') $member
}

function Get-FileSha256 {
    param([string]$Path)
    return (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash
}

$stats = @{
    Moved     = 0
    Removed   = 0
    Conflicts = 0
    Skipped   = 0
}

$conflicts = New-Object System.Collections.Generic.List[string]

if (-not (Test-Path -LiteralPath $SourceRoot)) {
    Write-Error "Origem nao encontrada: $SourceRoot"
}

$files = Get-ChildItem -LiteralPath $SourceRoot -Recurse -Filter '*.cpy' -File
Write-Host "Encontrados $($files.Count) arquivos .cpy em Stefanini"

foreach ($file in $files) {
    $dest = Get-DestinationPath -FileName $file.Name
    $destDir = Split-Path -Parent $dest

    if (-not (Test-Path -LiteralPath $destDir)) {
        if ($WhatIf) {
            Write-Host "[WhatIf] Criar pasta $destDir"
        }
        else {
            New-Item -ItemType Directory -Path $destDir -Force | Out-Null
        }
    }

    if (-not (Test-Path -LiteralPath $dest)) {
        if ($WhatIf) {
            Write-Host "[WhatIf] Mover $($file.FullName) -> $dest"
        }
        else {
            Move-Item -LiteralPath $file.FullName -Destination $dest
        }
        $stats.Moved++
        continue
    }

    $srcHash = Get-FileSha256 -Path $file.FullName
    $destHash = Get-FileSha256 -Path $dest

    if ($srcHash -eq $destHash) {
        if ($WhatIf) {
            Write-Host "[WhatIf] Apagar duplicata $($file.FullName) (identico a LIBS)"
        }
        else {
            Remove-Item -LiteralPath $file.FullName -Force
        }
        $stats.Removed++
        continue
    }

    $relSrc = $file.FullName.Substring($RepoRoot.Path.Length + 1)
    $relDest = $dest.Substring($RepoRoot.Path.Length + 1)
    $conflicts.Add("- **$($file.Name)** | origem: ``$relSrc`` | LIBS: ``$relDest``")
    $stats.Conflicts++
}

$report = @"
# Conflitos migrate-stef-copy-to-libs

Gerado: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss')

Copybooks Stefanini com conteudo **diferente** do member ja existente em LIBS.
LIBS foi mantido; origem Stefanini **nao** foi removida.

Total de conflitos: $($stats.Conflicts)

$(
    if ($conflicts.Count -eq 0) {
        '_Nenhum conflito._'
    }
    else {
        ($conflicts -join "`n")
    }
)
"@

if (-not $WhatIf) {
    Set-Content -LiteralPath $ReportPath -Value $report -Encoding UTF8
}

Write-Host ''
Write-Host 'Resumo:'
Write-Host "  Movidos:     $($stats.Moved)"
Write-Host "  Duplicatas:  $($stats.Removed) (apagadas em Stefanini)"
Write-Host "  Conflitos:   $($stats.Conflicts)"
if (-not $WhatIf -and $stats.Conflicts -gt 0) {
    Write-Host "  Relatorio:   $ReportPath"
}
