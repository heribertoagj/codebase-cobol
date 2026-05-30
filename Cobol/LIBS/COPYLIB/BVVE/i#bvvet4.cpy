      *================================================================*
      *  I#BVVET4 - AREA DE COMUNICACAO COM MODULO BVVE0375            *
      *             ATUALIZA GARANTIA DE MOVIMENTO E LIMITE NO DB2     *
      *----------------------------------------------------------------*
      *  LRECL: 172                                                    *
      *================================================================*
       01  BVVET4-COMMAREA.
           05 BVVET4-HEADER.
              10 BVVET4-COD-LAYOUT         PIC  X(008) VALUE 'I#BVVET4'.
              10 BVVET4-TAM-LAYOUT         PIC  9(005) VALUE 172.
      *
           05 BVVET4-BLOCO-REGISTRO.
              10 BVVET4-BLOCO-ENTRADA.
                 15 BVVET4-E-AGENCIA       PIC S9(005)    COMP-3.
                 15 BVVET4-E-CONTA         PIC S9(007)    COMP-3.
                 15 BVVET4-E-PRODUTO       PIC S9(005)    COMP-3.
                 15 BVVET4-E-DMOVTO        PIC  X(010).
                 15 BVVET4-E-GAR-MOVTO     PIC S9(013)V99 COMP-3.
                 15 BVVET4-E-GAR-LIMITE    PIC S9(013)V99 COMP-3.
                 15 FILLER                 PIC  X(069).
      *
              10 BVVET4-BLOCO-SAIDA.
                 15 BVVET4-S-COD-RETORNO   PIC  9(002).
                 15 BVVET4-S-MSG-RETORNO   PIC  X(052).
      *================================================================*
