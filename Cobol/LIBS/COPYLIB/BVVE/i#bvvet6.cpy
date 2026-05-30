      *================================================================*
      *  I#BVVET6 - AREA DE COMUNICACAO COM O MODULO BVVE0377          *
      *             OBTENCAO DE CONTA VINCULADA 7900                   *
      *----------------------------------------------------------------*
      *  LRECL: 124                                                    *
      *----------------------------------------------------------------*
      *  BVVET6-E-BANDEIRA - BCPP VISAO 2                              *
      *  BVVET6-S-COD-RETORNO: 00 - OK                                 *
      *                        08 - ERRO CONSISTENCIA                  *
      *                        16 - ERRO CICS / DB2                    *
      *================================================================*
       01  BVVET6-COMMAREA.
           05 BVVET6-HEADER.
              10 BVVET6-COD-LAYOUT         PIC  X(008) VALUE 'I#BVVET6'.
              10 BVVET6-TAM-LAYOUT         PIC  9(005) VALUE 124.
      *
           05 BVVET6-BLOCO-REGISTRO.
              10 BVVET6-BLOCO-ENTRADA.
                 15 BVVET6-E-AGEN-DOM      PIC  9(005).
                 15 BVVET6-E-CONTA-DOM     PIC  9(013).
                 15 BVVET6-E-BAND-DOM      PIC  9(003).
      *
              10 BVVET6-BLOCO-SAIDA.
                 15 BVVET6-E-AGEN-VINC     PIC  9(005).
                 15 BVVET6-E-CONTA-VINC    PIC  9(013).
      *
                 15 BVVET6-S-COD-RETORNO   PIC  9(002).
                 15 BVVET6-S-MSG-RETORNO   PIC  X(070).
