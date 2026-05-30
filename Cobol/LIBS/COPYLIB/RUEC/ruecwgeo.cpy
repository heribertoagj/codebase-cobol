      *================================================================*
      *                    B S I  T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    COPYBOOK....:   RUECWGEO                                    *
      *    ANALISTA....:   BSI TECNOLOGIA                              *
      *    DATA........:   DEZEMBRO/2015                               *
      *    OBJETIVO....:   INDICAR, COM BASE NOS DADOS DA CEDULA,      *
      *                    SE O CADASTRAMENTO DE COORDENADAS GEODESICAS*
      *                    EH OBRIGATORIO.                             *
      *----------------------------------------------------------------*
      *
       01  RUECWGEO-COPYBOOK.
        05 RUECWGEO-HEADER.
           10 RUECWGEO-COD-LAYOUT      PIC X(08)    VALUE 'RUECWGEO'.
           10 RUECWGEO-TAM-LAYOUT      PIC 9(05)    VALUE 00118.
        05 RUECWGEO-REGISTRO.
           10 RUECWGEO-BLOCO-RETORNO.
              15 RUECWGEO-COD-RETORNO               PIC 9(002).
              15 RUECWGEO-MENSAGEM                  PIC X(079).
           10 RUECWGEO-BLOCO-ENTRADA.
              15 RUECWGEO-E-CJUNC-DEPDC             PIC 9(005).
              15 RUECWGEO-E-CCDULA                  PIC 9(009).
              15 RUECWGEO-E-CCONTR                  PIC 9(009).
              15 RUECWGEO-E-EXCLUIR                 PIC X(001).
           10 RUECWGEO-BLOCO-SAIDA.
              15 RUECWGEO-COORD-OBRIG               PIC X(001).
