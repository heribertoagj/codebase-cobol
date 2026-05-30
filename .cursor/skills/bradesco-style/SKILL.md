---
name: bradesco-style
description: Aplica o padrao visual e estrutural de programas COBOL legados do Bradesco nesta workspace. Use ao criar, editar ou completar fontes COBOL — cabecalhos, SECTION, indentacao col 20/40, 1000/2000/3000 em programas novos. Referencia completa em Cobol/Projetos/_indice/referencia-cobol/.
---

# Bradesco Style

## Objetivo

Reproduzir estilo COBOL Bradesco desta workspace (referencia: `RUCA7951`, `RUEC7951`).

**Documentacao completa:** `Cobol/Projetos/_indice/referencia-cobol/`

| Capitulo | Conteudo |
|---|---|
| [01-estrutura-programa.md](../../../Cobol/Projetos/_indice/referencia-cobol/01-estrutura-programa.md) | SECTION, 1000/2000/3000, col 20/40 |
| [02-comentarios-molduras.md](../../../Cobol/Projetos/_indice/referencia-cobol/02-comentarios-molduras.md) | Molduras, FILLER |
| [04-condicionais-fluxo.md](../../../Cobol/Projetos/_indice/referencia-cobol/04-condicionais-fluxo.md) | IF, GO TO, PERFORM |
| [08-copybook-data-division.md](../../../Cobol/Projetos/_indice/referencia-cobol/08-copybook-data-division.md) | Layout COR/CIR |

## Regras rapidas (desenvolvimento)

1. Tudo em **MAIUSCULAS**; comentarios coluna fixa com `*`.
2. Toda logica em **SECTION** com `NNNN-99-FIM. EXIT.`
3. **Programa novo:** `1000-INICIALIZAR`, `2000-PROCESSAR`, `3000-FINALIZAR`.
4. **Indentacao (trecho novo):** verbo col **20**, complemento col **40**; aninhado **+3**; quebra na linha de baixo alinhada.
5. `GO TO` so na mesma SECTION; outras SECTIONs via `PERFORM`.
6. Preservar consultoria do fonte (`7 C O M M`, `S T E F A N I N I`).
7. Alteracoes em fonte existente: ver cap. **03** (`bradesco-quality-code`).

## SICOR

Layout COR/CIR: cap. 08 + `agroleg-knowledge/initiatives/SICOR-5.12.md`.

## Combinar com

- Alteracoes / rastreio: `bradesco-quality-code` → cap. 03
- Pre-subida: `demanda-pre-subida` → cap. 10
- Fluxo AGROLEG Fase 4: `demanda-workflow`

## Checklist resumido

- [ ] Cap. 01 — estrutura e indentacao
- [ ] Cap. 02 — molduras
- [ ] Cap. 04 — fluxo PERFORM/GO TO
- [ ] Cap. 08 — copybook SICOR se aplicavel
