# Modulo CMRU (Areas compartilhadas / middleware)

## Papel no negocio

- Copybooks e areas compartilhadas entre modulos RUCA, RUEC e fluxos CICS.

## Mapa tecnico

| Artefato | Papel | Observacao |
|---|---|---|
| CMRUW08I, CMRUW09I | Areas CICS | AGROLEG-993 |
| CMRUWA0A, CMRUWAKC | Areas compartilhadas | AGROLEG-993 |
| I#MIDD01 | Middleware | Dependencia LIBS |

## Historias AGROLEG neste modulo

| Historia | Escopo | Relacao |
|---|---|---|
| AGROLEG-993 | Dependencias em telas RUCA/RUEC | mesmo-modulo |

## Pontos sensiveis (impacto)

- Copybooks CMRU* frequentemente compartilhados — alteracao pode impactar multiplos programas.

## Licoes registradas

- (a expandir conforme novas historias)
