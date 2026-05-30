---
name: readme-dependencias-libs
description: Gera e atualiza a secao Dependencias LIBS de demandas COBOL comparando COPY e EXEC SQL INCLUDE com arquivos da pasta LIBS. Use ao iniciar demanda AGROLEG, preencher README, mapear copybooks, identificar dependencias ou listar pendencias de copybook/dclgen.
---

# README Dependencias LIBS

## Objetivo
Gerar um bloco Markdown padrao para a secao `Dependencias LIBS` do README da demanda.

## Quando usar
- Usuario pedir para preencher/atualizar dependencias no README.
- Usuario pedir comparacao entre fontes da demanda e pasta `Cobol/LIBS`.
- Usuario pedir lista de dependencias faltantes.

## Entradas
- Caminho da demanda (ex.: `Cobol/Projetos/Capgemini/AGROLEG-993`).
- Caminho da pasta de bibliotecas (ex.: `Cobol/LIBS`).

## Fluxo
1. Preview (stdout):
   - `powershell -ExecutionPolicy Bypass -File ".cursor/skills/readme-dependencias-libs/scripts/gerar_dependencias.ps1" -DemandaPath "<caminho_demanda>" -LibsPath "<caminho_libs>"`
2. Atualizar README automaticamente:
   - `powershell -ExecutionPolicy Bypass -File ".cursor/skills/readme-dependencias-libs/scripts/gerar_dependencias.ps1" -DemandaPath "<caminho_demanda>" -LibsPath "Cobol/LIBS" -ReadmePath "<readme>" -InPlace`
3. O script varre fontes da demanda (`.cbl`, `.cpy`, `.mfs`, `.sql`, `.jcl`).
4. Extrai:
   - `COPY <nome>`
   - `EXEC SQL INCLUDE <nome>`
5. Compara com arquivos `.cpy` em `Cobol/LIBS`.
6. Classifica em:
   - COPYBOOKS encontrados.
   - DCLGEN encontrados.
   - Pendencias nao encontradas.
7. Com `-InPlace`, substituir secao `## Dependencias LIBS` no README (ou inserir antes de Riscos/Dados Jira).
8. Copybooks locais na pasta da demanda sao listados com caminho relativo quando ausentes em Cobol/LIBS.

## Integracao

- Pipeline: `demanda-workflow`
- Analise de impacto: `agroleg-knowledge`

## Regras
- Nao inventar nomes de dependencias.
- Nao marcar como encontrado sem arquivo correspondente em `Cobol/LIBS`.
- Tratar comparacao sem diferenciar maiuscula/minuscula.
- Dependencias comentadas (linha iniciando com `*`) nao entram.
- `SQLCA` pode ser listado como "sistema" quando nao existir em `Cobol/LIBS`.

## Formato de saida (obrigatorio)
Usar exatamente este formato no README:

```markdown
## Dependencias LIBS
- COPYBOOKS (encontradas):
  - NOME -> Cobol/LIBS/COPYLIB/...
- DCLGEN (encontradas):
  - NOME -> Cobol/LIBS/DCLGEN/...
- Pendencias (nao encontradas em LIBS):
  - NOME
- Dependencias de sistema:
  - SQLCA
```
