# Modulo RUEC (Credito Rural / Reneg / SICOR)

## Papel no negocio

- Integracao SICOR (mensageria COR), renegociacao, liberacao batch de contratos, telas online de manutencao rural.

## Mapa tecnico

| Artefato | Papel | Observacao |
|---|---|---|
| RUEC8831 | Montador da COR0005 | Exclusao campos 5.12 — AGROLEG-1239 |
| RUEC0895 | Batch liberacao contratos | Orquestrador; sem alteracao prevista em 1239 |
| I#RUECC5 | Layout copybook COR0005 | Hierarquia completa; exclusao com CGaamm* (ex.: CG2605*) |
| RUEC7951 | Renegociacao online | AGROLEG-993 — calculos e encargos |
| RUEC7000 | Tela manutencao | AGROLEG-993 — Tipo Exploracao habilitado |
| RUECT700 | Mapa BMS RUEC7000 | AGROLEG-993 |
| RUECB00C | DCLGEN | TCDULA_TEMPR_RENEG (rule dclgen-sql-validation) |

## Mensagens SICOR deste modulo

| Mensagem | Programas/copybooks tipicos | Historias |
|---|---|---|
| COR0005 | RUEC8831, I#RUECC5 | AGROLEG-1239, AGROLEG-1109 |

Iniciativa regulatória: `initiatives/SICOR-5.12.md`.

## Malha batch conhecida

| Fluxo | JSON | HTML | Demanda |
|---|---|---|---|
| Liberacao COR0005 RURA | `referencia-batch-malha/malhas/AGROLEG-1239-liberacao.json` | `.../output/AGROLEG-1239-liberacao.html` | AGROLEG-1239 |

## Historias AGROLEG neste modulo

| Historia | Escopo | Relacao |
|---|---|---|
| AGROLEG-993 | Ajustes renegociacao online (RUEC7951, RUEC7000) | base reneg |
| AGROLEG-1239 | COR0005 batch — exclusao certificacao/rastreabilidade 5.12 | mesma-mensagem com 1109 |
| AGROLEG-1109 | Levantamento SICOR 5.12 consolidado RUEC/RUCA/CRUR | mesma-mensagem COR0005 |

## Pontos sensiveis (impacto)

- Alteracao em I#RUECC5 afeta serializacao em RUEC8831 e processos online que geram COR0005.
- Campos removidos na 5.12: comentar hierarquia completa no copybook; invalidar verbos (`-IF`, `-MOVE`).
- Subtarefas online da 1239 (1392, 1395, ...) compartilham layout COR0005.
- RUEC7951 concentra logica de reneg — impacto em calculos afeta telas RUCA vinculadas.

## Licoes registradas

- COR0005 5.12: nao reutilizar ordem de tags da 5.11.
- Exclusao SICOR: campos permanecem em telas RUCA/RUEC/CRUR conforme negocio — so saem da mensageria.
