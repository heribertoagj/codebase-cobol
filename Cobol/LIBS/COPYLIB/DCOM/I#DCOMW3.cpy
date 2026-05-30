      ******************** R O T I N A   R I S C *********************
      *                                                              *
      * ARQUIVO DE CONTRATOS RENEGOCIADOS - GBEL                     *
      *                                                              *
      * INC   I#DCOMW3           LRECL=170                           *
      ****************************************************************
      *
       01 REG-RISC-GBEL-RENOG.
          05 REN-AGENCIA-ORIGEM       PIC  9(005).
          05 REN-CONTA-ORIGEM         PIC  9(013).
          05 REN-CARTEIRA-ORIGEM      PIC  X(003).
          05 REN-CONTR-ORIGEM         PIC  9(017).
          05 REN-AGENCIA-NOVO         PIC  9(005).
          05 REN-CONTA-CORRENTE-NOVO  PIC  9(013).
          05 REN-CARTEIRA-NOVO        PIC  X(003).
          05 REN-CONTRATO-NOVO        PIC  9(017).
          05 REN-VR-RENEGOCIACAO      PIC  9(015)V99.
          05 REN-GRUPO-CONTABIL       PIC  9(002).
          05 REN-SUB-GRUPO-CONTABIL   PIC  9(002).
          05 REN-EMPRESA              PIC  9(005).
          05 REN-CPF                  PIC  9(015).
          05 REN-CENTRO-CUSTO-ORIGEM  PIC  X(004).
          05 FILLER                   PIC  X(049).
