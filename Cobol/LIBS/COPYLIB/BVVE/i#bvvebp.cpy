      *----------------------------------------------------------------*
      *  GCVI - CONTROLE DE INSUFICIENCIAS                             *
      *  I#BVVEBP - CONTAS COM INSUFICIENCIA DE SALDOS P BLOQUEIO      *
      *  TAMANHO: 300                                                  *
      *----------------------------------------------------------------*

       01  BVVEBP-REGISTRO.
           03 BVVEBP-AGENCIA            PIC 9(005).
           03 BVVEBP-CONTA              PIC 9(013).
           03 BVVEBP-BANDEIRA           PIC 9(003).
           03 BVVEBP-VLR-BLQ-SOLIC      PIC 9(013)V99 COMP-3.
           03 BVVEBP-VLR-BLOQUEADO      PIC 9(013)V99 COMP-3.
           03 BVVEBP-VLR-A-TRANSF       PIC 9(013)V99 COMP-3.
           03 BVVEBP-DT-MOVTO           PIC 9(008).
           03 BVVEBP-AGENCIA-7900       PIC 9(005).
           03 BVVEBP-CONTA-7900         PIC 9(013).
           03 FILLER                    PIC X(137).
      *    DATA: AAAAMMDD
