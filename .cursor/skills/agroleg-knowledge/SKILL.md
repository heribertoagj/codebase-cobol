---
name: agroleg-knowledge
description: Consulta e captura conhecimento acumulado de historias AGROLEG — iniciativas de negocio (ex. SICOR 5.12), relacoes entre demandas, mapas de modulos RU*/RCOR/CMRU/CRUR e licoes para analise de impacto. Use ao analisar impacto tecnico, relacionar historias, demanda SICOR ou registrar conhecimento ao fechar implementacao.
---

# AGROLEG Knowledge (Acervo)

## Objetivo

Reutilizar entendimento de modulos, iniciativas de negocio e relacoes entre historias para tornar analises de impacto mais precisas.

## Arquivos do acervo

| Arquivo | Conteudo |
|---|---|
| `index.md` | Indice global de historias, mensagens SICOR e modulos |
| `initiatives/SICOR-5.12.md` | Demanda/epic SICOR 5.12 (catalogo, checklist, historias) |
| `modules/RUEC.md` | Mapa tecnico modulo RUEC |
| `modules/RUCA.md` | Mapa tecnico modulo RUCA |
| `modules/RCOR.md` | Mapa tecnico modulo RCOR |
| `modules/CRUR.md` | Mapa tecnico modulo CRUR |
| `modules/CMRU.md` | Mapa tecnico modulo CMRU |

Regenerar indice a partir dos READMEs:

```powershell
powershell -ExecutionPolicy Bypass -File ".cursor/skills/agroleg-knowledge/scripts/rebuild_index.ps1" -ProjetosPath "Cobol/Projetos"
```

## Iniciativas vs modulos

| Tipo | Exemplo | Conteudo |
|---|---|---|
| **Iniciativa** | SICOR 5.12 | Requisito de negocio, catalogo, checklist regulatorio, historias da epic |
| **Modulo** | RUEC | Onde no Bradesco (programas, copybooks, sections) |
| **Historia** | AGROLEG-1239 | README da demanda — escopo especifico desta entrega |

Demandas de negocio (como SICOR 5.12) ficam em `initiatives/`, **nao** em skills permanentes.

## Momento A — Consulta (antes de codar)

1. Ler `index.md`.
2. Se README ou titulo indicar SICOR 5.12 / COR / CIR: ler `initiatives/SICOR-5.12.md`.
3. Ler modulos candidatos (prefixo RU*, RCOR, titulo Jira).
4. Ler README de historias relacionadas em `Cobol/Projetos/**/AGROLEG-*/README.md`.
5. Buscar por: mesma mensagem, mesmo programa/copybook, mesma tabela, subtarefas citadas.
6. Produzir bloco **Impacto sugerido com base no acervo**:

```text
INICIATIVA
- SICOR 5.12: [sim/nao] — historias relacionadas

HISTORIAS RELACIONADAS
- AGROLEG-xxx [relacao]: ...

SUSPEITOS DE IMPACTO (confirmar no fonte)
- Programa:
- Copybook:
- DCLGEN/tabela:

RISCOS CONHECIDOS
- (modulo ou iniciativa)

CHECKLIST 5.12 (se aplicavel)
- Ver initiatives/SICOR-5.12.md

PROXIMA ACAO
- Validar no fonte e preencher README (preliminar)
```

7. Cruzar com `readme-dependencias-libs` e rule `dclgen-sql-validation`.

Integracao com `demanda-workflow`:
- Fase 1: impacto preliminar + fontes a baixar
- Fase 2: analise iterativa ate `ANALISE CONCLUIDA`
- Fase 7: captura no acervo

## Momento B — Captura (ao fechar historia)

1. Completar no README: impacto confirmado, decisoes, historias relacionadas, licoes.
2. Atualizar `modules/<PREFIXO>.md` — onde no Bradesco.
3. Se historia compoe SICOR 5.12: atualizar tabela em `initiatives/SICOR-5.12.md`.
4. Executar `rebuild_index.ps1`.
5. Apenas fatos confirmados — nao suposicao.

## Tipos de relacao entre historias

| Rotulo | Significado |
|---|---|
| `pre-requisito` | Deve estar em producao antes |
| `complementa` | Mesmo fluxo, escopo adicional |
| `mesma-mensagem` | COR/CIR em comum |
| `mesmo-programa` | Altera o mesmo `.cbl` |
| `mesma-tabela` | Mesmo DCLGEN/tabela DB2 |
| `mesmo-modulo` | Mesmo centro de custo |
| `substitui` | Nova abordagem revoga a anterior |
| `conflita` | Alteracoes incompativeis em paralelo |

## Regras

- Requisito de negocio: `## Requisito de negocio` no README (iniciativa + mensagens desta historia).
- Detalhes regulatorios 5.12: `initiatives/SICOR-5.12.md`.
- Onde no Bradesco: `modules/*.md`.
- Regras COBOL/Hexavision: `bradesco-quality-code` (nao duplicar aqui).

## Integracao

- Pipeline: `demanda-workflow` (Fases 1, 2 e 7)
- Template README: `readme-demanda-template`
- Pre-subida: `demanda-pre-subida` (Fase 5)
