---
name: malha-batch-analise
description: Analisa e documenta malha batch AGROLEG — dependencias JOB/JCL/programa/arquivo, JSON de malha e HTML visual. Use ao analisar fluxo batch, JCL em jcl/, impacto entre jobs ou gerar diagrama de malha.
---

# Malha Batch — Analise

## Objetivo

Documentar e visualizar dependencias entre **JOBs**, **JCLs**, **programas** e **arquivos** em fluxos batch.

## Referencia

- README: `Cobol/Projetos/_indice/referencia-batch-malha/README.md`
- Schema: `Cobol/Projetos/_indice/referencia-batch-malha/schema-malha.md`
- Template: `Cobol/Projetos/_indice/referencia-batch-malha/template-malha.json`

## Quando usar

- Demanda com escopo **BATCH** (Fase 2 do `demanda-workflow`).
- Usuario pede malha batch, diagrama de jobs, analise de JCL.
- Impacto entre programas via arquivos (LRECL, DDNAME).

## Fluxo

1. Ler JCLs em `Cobol/Projetos/.../AGROLEG-nnn/jcl/` (ou informados pelo usuario).
2. Extrair: `//JOB`, `//STEP EXEC PGM=`, `//DD DSN=`, `DCB LRECL=`, `BLKSIZE=`.
3. Preencher/atualizar JSON: `malhas/<demanda>-<fluxo>.json`.
4. Gerar HTML:
   ```powershell
   powershell -ExecutionPolicy Bypass -File "Cobol/Projetos/_indice/referencia-batch-malha/scripts/gerar_malha_html.ps1" -MalhaPath "Cobol/Projetos/_indice/referencia-batch-malha/malhas/AGROLEG-nnn-<fluxo>.json"
   ```
5. Atualizar README da demanda — secao **Malha batch**.
6. Se aplicavel: secao em `agroleg-knowledge/modules/<MODULO>.md`.

## Campos manuais (scheduler)

Dependencia JOB→JOB via Control-M / OPC **nao** aparece no JCL — informar em:

- `dispara`: array de IDs de JOB filhos
- `chamado_por`: ID do JOB pai

## Resumo por programa

Para cada step, preencher `resumo` com 1-2 frases (funcao de negocio). Cruzar com acervo e README de demandas relacionadas.

## Saida esperada

```text
=== MALHA BATCH: AGROLEG-nnn ===

FONTE JSON: malhas/....json
HTML: output/....html

JOBS
- JOB-A → JOB-B (dispara)
- Programas: ...
- Arquivos criticos: ...

IMPACTO
- Alteracao em PGM X afeta DD Y (LRECL n)
```

## Integracao

- `demanda-workflow` Fase 2
- `referencia-cobol/07-arquivos-file-status.md` (FILE STATUS nos PGMs)
- `agroleg-knowledge`
