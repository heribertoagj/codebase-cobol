# Modulo RCOR (Processamento mensageria COR)

## Papel no negocio

- Processamento e roteamento de mensagens COR do ecossistema credito rural.

## Mapa tecnico

| Artefato | Papel | Observacao |
|---|---|---|
| RCOR5050 | Processamento COR | AGROLEG-1243 |
| RCOR6006 | Processamento COR | AGROLEG-1243 |
| RCOR6007 | Processamento COR | AGROLEG-1243 |
| RCOR6010 | Processamento COR | AGROLEG-1243 |
| RCOR8010 | Processamento COR | AGROLEG-1243 |
| RCOR8015 | Processamento COR | AGROLEG-1243 |
| RCORB001 | Processamento COR | AGROLEG-1243 |
| RCORB002 | Processamento COR | AGROLEG-1243 |
| TMNF5026 | Processamento relacionado | AGROLEG-1243 |

## Mensagens SICOR deste modulo

| Mensagem | Historias |
|---|---|
| COR0001, COR0005, COR0006 | AGROLEG-1109 (levantamento) |
| (a confirmar) | AGROLEG-1243 |

## Historias AGROLEG neste modulo

| Historia | Escopo | Relacao |
|---|---|---|
| AGROLEG-1243 | Fontes RCOR* em pasta pgm | em andamento |
| AGROLEG-1109 | Levantamento SICOR 5.12 | mesma-mensagem |

## Pontos sensiveis (impacto)

- Alteracoes de layout COR0005 em RUEC podem exigir ajuste em cadeia RCOR.
- Validar dependencias I#RCOR* ao analisar impacto.

## Licoes registradas

- (a registrar ao concluir AGROLEG-1243)
