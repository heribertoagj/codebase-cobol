# Referencia — Malha Batch (JOB / JCL / Programa / Arquivo)

Documentacao e visualizacao de dependencias entre **JOBs**, **JCLs**, **programas COBOL** e **arquivos** em fluxos batch.

## Estrutura

```
referencia-batch-malha/
├── README.md                 ← este arquivo
├── schema-malha.md           ← definicao dos campos
├── template-malha.json       ← modelo vazio
├── malhas/                   ← uma malha por fluxo/demanda
│   └── AGROLEG-1239-liberacao.json
├── scripts/
│   └── gerar_malha_html.ps1
└── output/                   ← HTML gerado (abrir no browser)
    └── AGROLEG-1239-liberacao.html
```

## Fluxo de uso

1. Na **Fase 2** da demanda AGROLEG (analise batch), preencher ou atualizar JSON em `malhas/`.
2. Gerar HTML visual:
   ```powershell
   powershell -ExecutionPolicy Bypass -File "Projetos/_indice/referencia-batch-malha/scripts/gerar_malha_html.ps1" -MalhaPath "Projetos/_indice/referencia-batch-malha/malhas/AGROLEG-1239-liberacao.json"
   ```
3. Abrir `output/<nome>.html` no browser.
4. Registrar no README da demanda (secao **Malha batch**).

## Formato fonte

- **JSON** (`.json`) — formato oficial para o script (PowerShell nativo).
- Campos documentados em `schema-malha.md`; equivalente conceitual ao YAML discutido na analise.

## O que o HTML exibe

- Diagrama Mermaid: JOB → JOB (dispara) e arquivos (I/O).
- Tabela resumo por JOB / JCL / programas.
- Cards detalhados: steps, programas, resumo, arquivos (DDNAME, operacao, LRECL).

## Limitacoes

- Dependencia JOB→JOB via **scheduler** (Control-M / OPC) deve ser informada manualmente em `dispara` / `chamado_por`.
- HTML usa **Mermaid via CDN** — requer internet ao abrir (ou salvar HTML ja renderizado pelo browser).

## Integracao

- Skill: `.cursor/skills/malha-batch-analise`
- Fluxo AGROLEG Fase 2: `demanda-workflow`
- Acervo: `agroleg-knowledge/modules/*.md` (secao Malha batch)

## Comando para o agente

> Analisar malha batch AGROLEG-nnn a partir dos JCL em jcl/ e gerar HTML
