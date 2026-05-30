# AGROLEG-993

## Objetivo
- Eu como PO
- Ajustar o cálculo do valor renegociado e dos encargos na tela "VENCIMENTO(S) DO CONTRATO - A VENCER E VENCIDOS", garantindo o arredondamento correto conforme dados do contrato no RUCA .
- Corrigir o comportamento da tela "DADOS DOS CONTRATOS PARA REORGANIZAÇÃO" da rotina RUCA, para que, ao pressionar PF8, o sistema exiba a mensagem "NÃO HÁ MAIS REGISTROS" no rodapé quando não houver dados disponíveis, em vez de avançar para uma tela em branco .
- Na tela RUEC7000 deixar o campo Tipo de Exploração habilitado para permitir sua alteração após retornar da tela de imóvel , para operações que exigem imóvel . OBS. Erro apresentado em teste regressivo na história AGROLEG-259 - RENEGOCIAÇÃO - Inclusão - Tratar produto 000 para não solicitar imóvel.
- Garantir que os valores de renegociação e encargos sejam ajustados corretamente na tela "VENCIMENTO(S) DO CONTRATO - A VENCER E VENCIDOS", aplicando o arredondamento adequado, assegurando precisão e consistência nos cálculos apresentados ao usuário.
- Melhorar a usabilidade da tela "DADOS DOS CONTRATOS PARA REORGANIZAÇÃO" na rotina RUCA, exibindo uma mensagem clara no rodapé ("NÃO HÁ MAIS REGISTROS") quando não houver dados disponíveis, ao pressionar PF8.
- Garantir a correta funcionalidade do campo Tipo de Exploração para operações que exigem imóvel .

## Centros de custo impactados
- RUCA
- RUEC

## Pacores changeman 

- RUCA000743 / TICKET-77124 (REVERT RUCA000750)
- RUEC001588 (CNPJ)
- Change para implantação CHG000920074

## Escopo tecnico
- COBOL
- JCL
- CICS
- DB2
- IMS

## Fontes impactados (inicial)
- RUCA/pgm/ruca7602.cbl
- RUCA/pgm/ruca6690.cbl
- RUEC/pgm/ruec7951.cbl
- RUEC/pgm/ruec7000.cbl
- RUEC/mfs/ruect700.mfs

## Dependencias LIBS
- COPYBOOKS (encontradas):
  - CMRUW08I -> LIBS/COPYLIB/CMRU/cmruw08i.cpy
  - CMRUW09I -> LIBS/COPYLIB/CMRU/cmruw09i.cpy
  - CMRUWA0A -> LIBS/COPYLIB/CMRU/cmruwa0a.cpy
  - CMRUWAKC -> LIBS/COPYLIB/CMRU/cmruwakc.cpy
  - CRURW00W -> LIBS/COPYLIB/CRUR/crurw00w.cpy
  - CRURW236 -> LIBS/COPYLIB/CMRU/crurw236.cpy
  - CRURWG0I -> LIBS/COPYLIB/CMRU/crurwg0i.cpy
  - CRURWG2Y -> LIBS/COPYLIB/CMRU/crurwg2y.cpy
  - I#BRAD7C -> LIBS/COPYLIB/BRAD/I#BRAD7C.cpy
  - I#HEXA01 -> LIBS/COPYLIB/HEXA/i#hexa01.cpy
  - I#MIDD01 -> LIBS/COPYLIB/CMRU/i#midd01.cpy
  - I#RUEC00 -> LIBS/COPYLIB/RUEC/i#ruec00.cpy
  - I#RUEC08 -> LIBS/COPYLIB/RUEC/i#ruec08.cpy
  - I#RUEC20 -> LIBS/COPYLIB/CMRU/i#ruec20.cpy
  - I#RUEC21 -> LIBS/COPYLIB/CMRU/i#ruec21.cpy
  - I#RUEC22 -> LIBS/COPYLIB/CMRU/i#ruec22.cpy
  - I#RUEC31 -> LIBS/COPYLIB/CMRU/i#ruec31.cpy
  - I#RUEC58 -> LIBS/COPYLIB/CMRU/i#ruec58.cpy
  - POL7100C -> LIBS/COPYLIB/POOL/pol7100c.cpy
  - RUCAW602 -> LIBS/COPYLIB/RUCA/rucaw602.cpy
- DCLGEN (encontradas):
  - MESUV050 -> LIBS/DCLGEN/MESU/mesuv050.cpy
  - RUCAB003 -> LIBS/DCLGEN/RUCA/rucab003.cpy
  - RUCAB004 -> LIBS/DCLGEN/RUCA/rucab004.cpy
  - RUCAB022 -> LIBS/DCLGEN/RUCA/rucab022.cpy
  - RUECB007 -> LIBS/DCLGEN/RUEC/ruecb007.cpy
  - RUECB00C -> LIBS/DCLGEN/RUEC/ruecb00c.cpy
  - RUECB00D -> LIBS/DCLGEN/RUEC/ruecb00d.cpy
  - RUECB041 -> LIBS/DCLGEN/RUEC/ruecb041.cpy
  - RUECB042 -> LIBS/DCLGEN/RUEC/ruecb042.cpy
  - RUECB045 -> LIBS/DCLGEN/RUEC/ruecb045.cpy
  - RUECB048 -> LIBS/DCLGEN/RUEC/ruecb048.cpy
  - RUECB056 -> LIBS/DCLGEN/RUEC/ruecb056.cpy
  - RUECB061 -> LIBS/DCLGEN/RUEC/ruecb061.cpy
  - RUECB09H -> LIBS/DCLGEN/RUEC/ruecb09h.cpy
  - RUECV001 -> LIBS/DCLGEN/RUEC/ruecv001.cpy
  - RUECV007 -> LIBS/DCLGEN/RUEC/ruecv007.cpy
  - RUECV008 -> LIBS/DCLGEN/RUEC/ruecv008.cpy
  - RUECV019 -> LIBS/DCLGEN/RUEC/ruecv019.cpy
  - RUFIB002 -> LIBS/DCLGEN/RUFI/rufib002.cpy
  - RUMOB002 -> LIBS/DCLGEN/RUMO/rumob002.cpy
  - RUMOV000 -> LIBS/DCLGEN/RUMO/rumov000.cpy
  - RURCB003 -> LIBS/DCLGEN/RURC/rurcb003.cpy
  - RURCB006 -> LIBS/DCLGEN/RURC/rurcb006.cpy
  - RURCB00A -> LIBS/DCLGEN/RURC/rurcb00a.cpy
  - RURCB051 -> LIBS/DCLGEN/RURC/rurcb051.cpy
  - RURCB082 -> LIBS/DCLGEN/RURC/rurcb082.cpy
  - RURCB083 -> LIBS/DCLGEN/RURC/rurcb083.cpy
  - RURCB085 -> LIBS/DCLGEN/RURC/rurcb085.cpy
  - RURCB087 -> LIBS/DCLGEN/RURC/rurcb087.cpy
  - RURCB088 -> LIBS/DCLGEN/RURC/rurcb088.cpy
  - RURCB089 -> LIBS/DCLGEN/RURC/rurcb089.cpy
  - RURCB090 -> LIBS/DCLGEN/RURC/rurcb090.cpy
  - RURCB091 -> LIBS/DCLGEN/RURC/rurcb091.cpy
  - RURCV000 -> LIBS/DCLGEN/RURC/rurcv000.cpy
  - RURCV003 -> LIBS/DCLGEN/RURC/rurcv003.cpy
  - RURCV004 -> LIBS/DCLGEN/RURC/rurcv004.cpy
  - RURCV014 -> LIBS/DCLGEN/RURC/rurcv014.cpy
  - RURCV022 -> LIBS/DCLGEN/RURC/rurcv022.cpy
  - RURCV040 -> LIBS/DCLGEN/RURC/rurcv040.cpy
  - SQLCA -> LIBS/DCLGEN/sqlca.cpy
- Pendencias (nao encontradas em LIBS):
  - I#RUEC07
- Dependencias de sistema:
  - Nenhuma

## Dados Jira (sync)
- Chave: AGROLEG-993
- Titulo: [Flash -RUEC] - RENEGOCIAção - AJUSTES
- Status: Em homologação
- Responsavel: TATIANA VAITKEVICIUS CARDOSO GIANNASI
- Atualizado em: Mon, 6 Apr 2026 09:36:45 -0300

## Status
- [x] Em andamento
- [ ] Em teste
- [ ] Pronto para subida
