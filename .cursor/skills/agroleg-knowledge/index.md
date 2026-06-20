# Indice AGROLEG (conhecimento acumulado)

> Regenerado manualmente ou via `scripts/rebuild_index.ps1`. Ultima atualizacao: seed inicial.

## Por mensagem SICOR

| Mensagem | Historias | Modulos |
|---|---|---|
| COR0005 | AGROLEG-1239, AGROLEG-1109 | RUEC, RUCA, RCOR |
| COR0001 | AGROLEG-1109 | RUEC, RUCA, RCOR, CRUR |
| COR0006 | AGROLEG-1109 | RCOR |
| N/A (renegociacao online) | AGROLEG-993 | RUCA, RUEC |

## Por iniciativa de negocio

| Iniciativa | Historias | Arquivo |
|---|---|---|
| SICOR 5.12 | AGROLEG-1109, AGROLEG-1239, subtarefas 1392+ | initiatives/SICOR-5.12.md |

## Por modulo

| Modulo | Historias | Arquivo |
|---|---|---|
| RUEC | AGROLEG-993, AGROLEG-1239, AGROLEG-1109 | modules/RUEC.md |
| RUCA | AGROLEG-993, AGROLEG-1109 | modules/RUCA.md |
| RCOR | AGROLEG-1243, AGROLEG-1109 | modules/RCOR.md |
| CRUR | AGROLEG-1109 | modules/CRUR.md |
| CMRU | AGROLEG-993 | modules/CMRU.md |

## Historias indexadas

| Historia | Status | Modulos | SICOR | Caminho README |
|---|---|---|---|---|
| AGROLEG-993 | Finalizada | RUCA, RUEC | N/A | Cobol/Projetos/Capgemini/Finalizada/AGROLEG-993/README.md |
| AGROLEG-1109 | Finalizada | RUEC, RUCA, RCOR, CRUR | COR0001, COR0005, COR0006 | (sem README na raiz — ver pasta Finalizada) |
| AGROLEG-1239 | Finalizada | RUEC | COR0005 5.12 | Cobol/Projetos/Capgemini/Finalizada/AGROLEG-1239/README.md |
| AGROLEG-1243 | Finalizada | RCOR | COR0001E | Cobol/Projetos/Capgemini/Finalizada/AGROLEG-1243/README.md |

## Subtarefas / dependencias entre historias

| Historia | Relacionada | Relacao |
|---|---|---|
| AGROLEG-1239 | AGROLEG-1392, 1395, 1396, 1400, 1402, 1403, 1405, 1412, 1418, 1424, 1446, 1595 | complementa (subtarefas COR0005 online) |
| AGROLEG-993 | AGROLEG-259 | complementa (regressao Tipo Exploracao) |
| AGROLEG-1239 | AGROLEG-1109 | mesma-mensagem (COR0005 SICOR 5.12) |

## Como manter

1. Ao fechar historia: atualizar README + modulo + esta tabela.
2. Rodar `rebuild_index.ps1` para sincronizar a partir dos READMEs.
3. Revisar entradas "a levantar" na proxima analise.
