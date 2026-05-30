param(
    [Parameter(Mandatory = $true)]
    [string]$MalhaPath,
    [Parameter(Mandatory = $false)]
    [string]$OutputPath
)

$ErrorActionPreference = "Stop"

if (-not (Test-Path -LiteralPath $MalhaPath)) {
    throw "MalhaPath nao encontrado: $MalhaPath"
}

function Escape-Html {
    param([string]$Text)
    if ([string]::IsNullOrEmpty($Text)) { return "" }
    return [System.Net.WebUtility]::HtmlEncode($Text)
}

function Mermaid-SafeId {
    param([string]$Id)
    return ($Id -replace '[^A-Za-z0-9_]', '_')
}

$malhaRoot = Split-Path (Split-Path $MalhaPath -Parent) -Parent
if (-not $OutputPath) {
    $baseName = [System.IO.Path]::GetFileNameWithoutExtension($MalhaPath)
    $outDir = Join-Path $malhaRoot "output"
    if (-not (Test-Path -LiteralPath $outDir)) {
        New-Item -ItemType Directory -Path $outDir -Force | Out-Null
    }
    $OutputPath = Join-Path $outDir "$baseName.html"
}

$jsonRaw = Get-Content -LiteralPath $MalhaPath -Raw -Encoding UTF8
$data = $jsonRaw | ConvertFrom-Json

$m = $data.malha
$jobs = @($data.jobs)
$arquivos = @()
if ($data.arquivos_compartilhados) {
    $arquivos = @($data.arquivos_compartilhados)
}

# --- Mermaid ---
$mermaid = New-Object System.Collections.Generic.List[string]
$mermaid.Add("flowchart TB")

foreach ($job in $jobs) {
    $nid = Mermaid-SafeId $job.id
    $label = "$($job.id)<br/>$($job.nome_job)<br/>JCL: $($job.jcl)"
    $label = $label -replace '"', "'"
    $mermaid.Add("    ${nid}[`"$label`"]")
}

foreach ($job in $jobs) {
    if (-not $job.dispara) { continue }
    $from = Mermaid-SafeId $job.id
    foreach ($target in @($job.dispara)) {
        $to = Mermaid-SafeId $target
        $mermaid.Add("    $from -->|dispara| $to")
    }
}

foreach ($arq in $arquivos) {
    $aid = Mermaid-SafeId $arq.id
    $lrecl = if ($arq.lrecl) { " LRECL=$($arq.lrecl)" } else { "" }
    $alabel = "$($arq.ddname)$lrecl"
    $alabel = $alabel -replace '"', "'"
    $mermaid.Add("    ${aid}[(`"$alabel`")]")
    foreach ($jid in @($arq.usado_por)) {
        $jn = Mermaid-SafeId $jid
        $mermaid.Add("    $jn -.->|arquivo| $aid")
    }
}

$mermaidText = $mermaid -join "`n"

# --- Tabela resumo ---
$summaryRows = New-Object System.Collections.Generic.List[string]
foreach ($job in $jobs) {
    $progs = ($job.steps | ForEach-Object { $_.programa }) -join ", "
    $ioList = New-Object System.Collections.Generic.List[string]
    foreach ($step in $job.steps) {
        foreach ($f in @($step.arquivos)) {
            $lr = if ($f.lrecl) { " LRECL=$($f.lrecl)" } else { "" }
            $ioList.Add("$($f.operacao) $($f.ddname)$lr")
        }
    }
    $io = ($ioList | Select-Object -Unique) -join "; "
    $summaryRows.Add(@"
    <tr>
      <td><strong>$(Escape-Html $job.id)</strong></td>
      <td>$(Escape-Html $job.jcl)</td>
      <td>$(Escape-Html $job.nome_job)</td>
      <td>$(Escape-Html $progs)</td>
      <td>$(Escape-Html $io)</td>
      <td>$(Escape-Html $job.resumo)</td>
    </tr>
"@)
}

# --- Cards por JOB ---
$cards = New-Object System.Collections.Generic.List[string]
foreach ($job in $jobs) {
    $stepsHtml = New-Object System.Collections.Generic.List[string]
    foreach ($step in $job.steps) {
        $filesHtml = New-Object System.Collections.Generic.List[string]
        foreach ($f in @($step.arquivos)) {
            $lr = if ($f.lrecl) { $f.lrecl } else { "-" }
            $bk = if ($f.blocos) { $f.blocos } else { "-" }
            $filesHtml.Add(@"
            <tr>
              <td>$(Escape-Html $f.ddname)</td>
              <td>$(Escape-Html $f.operacao)</td>
              <td>$lr</td>
              <td>$bk</td>
              <td>$(Escape-Html $f.dataset)</td>
              <td>$(Escape-Html $f.observacao)</td>
            </tr>
"@)
        }
        $stepsHtml.Add(@"
        <div class="step">
          <h4>Step $(Escape-Html $step.step) - <code>$(Escape-Html $step.programa)</code></h4>
          <p class="resumo">$(Escape-Html $step.resumo)</p>
          <table class="files">
            <thead><tr><th>DDNAME</th><th>Op</th><th>LRECL</th><th>BLKSIZE</th><th>Dataset</th><th>Obs</th></tr></thead>
            <tbody>$($filesHtml -join "`n")</tbody>
          </table>
        </div>
"@)
    }
    $dispara = if ($job.dispara -and @($job.dispara).Count -gt 0) {
        "Dispara: " + (($job.dispara) -join ", ")
    } elseif ($job.chamado_por) {
        "Chamado por: $($job.chamado_por)"
    } else { "" }

    $cards.Add(@"
    <article class="job-card" id="card-$(Mermaid-SafeId $job.id)">
      <header>
        <h3>$(Escape-Html $job.id) - $(Escape-Html $job.nome_job)</h3>
        <span class="meta">JCL: $(Escape-Html $job.jcl)</span>
        $(if ($dispara) { "<span class=`"meta link`">$(Escape-Html $dispara)</span>" })
      </header>
      <p class="resumo">$(Escape-Html $job.resumo)</p>
      $($stepsHtml -join "`n")
    </article>
"@)
}

$arqSection = ""
if ($arquivos.Count -gt 0) {
    $arqRows = foreach ($a in $arquivos) {
        $used = ($a.usado_por -join ", ")
        @"
        <tr>
          <td>$(Escape-Html $a.id)</td>
          <td>$(Escape-Html $a.ddname)</td>
          <td>$(if ($a.lrecl) { $a.lrecl } else { "-" })</td>
          <td>$(Escape-Html $used)</td>
          <td>$(Escape-Html $a.observacao)</td>
        </tr>
"@
    }
    $arqSection = @"
    <section>
      <h2>Arquivos compartilhados</h2>
      <table class="summary">
        <thead><tr><th>ID</th><th>DDNAME</th><th>LRECL</th><th>Usado por</th><th>Observacao</th></tr></thead>
        <tbody>$($arqRows -join "`n")</tbody>
      </table>
    </section>
"@
}

$html = @"
<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Malha Batch — $(Escape-Html $m.titulo)</title>
  <style>
    :root { --bg: #0f1419; --card: #1a2332; --border: #2d3a4d; --text: #e6edf3; --muted: #8b949e; --accent: #58a6ff; --ok: #3fb950; }
    * { box-sizing: border-box; }
    body { font-family: 'Segoe UI', system-ui, sans-serif; background: var(--bg); color: var(--text); margin: 0; padding: 1.5rem 2rem; line-height: 1.5; }
    h1 { font-size: 1.5rem; margin: 0 0 0.25rem; }
    .subtitle { color: var(--muted); margin-bottom: 1.5rem; }
    .badge { display: inline-block; background: var(--border); padding: 0.15rem 0.5rem; border-radius: 4px; font-size: 0.85rem; margin-right: 0.5rem; }
    section { margin-bottom: 2rem; }
    h2 { font-size: 1.15rem; border-bottom: 1px solid var(--border); padding-bottom: 0.35rem; margin-top: 0; }
    .mermaid-wrap { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem; overflow-x: auto; }
    table.summary { width: 100%; border-collapse: collapse; font-size: 0.9rem; }
    table.summary th, table.summary td { border: 1px solid var(--border); padding: 0.5rem 0.65rem; text-align: left; vertical-align: top; }
    table.summary th { background: var(--card); color: var(--accent); }
    table.summary tr:nth-child(even) { background: rgba(255,255,255,0.03); }
    .job-card { background: var(--card); border: 1px solid var(--border); border-radius: 8px; padding: 1rem 1.25rem; margin-bottom: 1rem; }
    .job-card header { display: flex; flex-wrap: wrap; align-items: baseline; gap: 0.75rem; margin-bottom: 0.5rem; }
    .job-card h3 { margin: 0; font-size: 1.1rem; color: var(--ok); }
    .meta { font-size: 0.85rem; color: var(--muted); }
    .meta.link { color: var(--accent); }
    .resumo { color: var(--muted); margin: 0.5rem 0 1rem; }
    .step { margin-top: 1rem; padding-top: 1rem; border-top: 1px dashed var(--border); }
    .step h4 { margin: 0 0 0.35rem; font-size: 1rem; }
    .step code { background: var(--bg); padding: 0.1rem 0.35rem; border-radius: 3px; }
    table.files { width: 100%; border-collapse: collapse; font-size: 0.85rem; margin-top: 0.5rem; }
    table.files th, table.files td { border: 1px solid var(--border); padding: 0.35rem 0.5rem; }
    table.files th { background: var(--bg); }
    footer { margin-top: 2rem; font-size: 0.8rem; color: var(--muted); }
  </style>
</head>
<body>
  <header>
    <h1>$(Escape-Html $m.titulo)</h1>
    <p class="subtitle">$(Escape-Html $m.descricao)</p>
    <span class="badge">$(Escape-Html $m.demanda)</span>
    <span class="badge">ID: $(Escape-Html $m.id)</span>
    <span class="badge">Atualizado: $(Escape-Html $m.atualizado_em)</span>
  </header>

  <section>
    <h2>Diagrama da malha</h2>
    <div class="mermaid-wrap">
      <pre class="mermaid">
$mermaidText
      </pre>
    </div>
    <p class="subtitle">Seta solida = JOB dispara JOB. Seta tracejada = arquivo compartilhado.</p>
  </section>

  <section>
    <h2>Resumo por JOB</h2>
    <table class="summary">
      <thead>
        <tr><th>JOB</th><th>JCL</th><th>Nome JOB</th><th>Programas</th><th>Arquivos (I/O)</th><th>Resumo</th></tr>
      </thead>
      <tbody>
        $($summaryRows -join "`n")
      </tbody>
    </table>
  </section>

  $arqSection

  <section>
    <h2>Detalhe por JOB e programa</h2>
    $($cards -join "`n")
  </section>

  <footer>
    Gerado de: $(Escape-Html $MalhaPath) — $(Get-Date -Format 'yyyy-MM-dd HH:mm')
  </footer>

  <script type="module">
    import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
    mermaid.initialize({
      startOnLoad: true,
      theme: 'dark',
      flowchart: { curve: 'basis', padding: 16 }
    });
  </script>
</body>
</html>
"@

$enc = New-Object System.Text.UTF8Encoding $false
[System.IO.File]::WriteAllText($OutputPath, $html, $enc)
Write-Output "HTML gerado: $OutputPath"
