# Fase 2 — Analise iterativa de fontes

**Disparo:** `Fontes baixados — continuar analise <ID>`

## Passos (cada rodada)

1. Ler fontes em `pgm/`, `cpy/`, `jcl/`, `mfs/` da demanda.
2. Extrair COPY, EXEC SQL INCLUDE, CALL, dependencias cruzadas.
3. Cruzar com `Cobol/LIBS`, acervo e README de historias relacionadas.
4. Rodar `readme-dependencias-libs` com `-InPlace` se houver fontes.
5. Atualizar `## Fontes impactados (preliminar)` e `## Fontes pendentes baixa`.
6. Skills condicionais: ver tabela "Skills filhas por tipo" em [SKILL.md](../SKILL.md).

## Conclusao da rodada (uma das tres)

**A) ANALISE PARCIAL — faltam fontes**

- Saida: [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-2--analise-parcial)
- README `## Status workflow` → `Analise em andamento — aguardando fontes`

**B) ANALISE CONCLUIDA**

- Saida: [templates/saidas-obrigatorias.md](../templates/saidas-obrigatorias.md#fase-2--analise-concluida)
- README: `## Impacto tecnico (confirmado)` (escopo analisado — refinado na implementacao)
- `## Fontes pendentes baixa`: Nenhuma
- `## Status workflow`: `Analise concluida — aguardando plano de ajuste`

**C) LACUNA DE NEGOCIO — fontes ok, requisito ambiguo**

- Nao pedir nova baixa de fonte.
- Acionar skill `clarification` (`Clarificar requisitos <ID>`).
- README `## Status workflow` → `Analise em andamento — aguardando clarificacao de requisitos`
- Apos clarificacao: retomar analise com `Fontes baixados — continuar analise <ID>`.

**Gate (padrao cliente):** nao avancar para Fase 3 sem declarar `ANALISE CONCLUIDA`.
