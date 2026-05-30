      *================================================================*
      *                    B S I  T E C N O L O G I A                  *
      *----------------------------------------------------------------*
      *    COPYBOOK....:   RUECW824                                    *
      *    ANALISTA....:   BSI TECNOLOGIA                              *
      *    DATA........:   JUNHO/2015                                  *
      *    OBJETIVO....:   OBTER CODIGO DO SUBPROGRAMA DO CONTRATO     *
      *----------------------------------------------------------------*
      *
       01  RUECW824-COPYBOOK.
        05 RUECW824-HEADER.
           10 RUECW824-COD-LAYOUT      PIC X(08)    VALUE 'RUECW824'.
           10 RUECW824-TAM-LAYOUT      PIC 9(05)    VALUE 00041.
        05 RUECW824-REGISTRO.
           10 RUECW824-BLOCO-RETORNO.
              15 RUECW824-COD-RETORNO               PIC 9(002).
           10 RUECW824-BLOCO-ENTRADA.
              15 RUECW824-E-CORIGE-REC              PIC 9(003).
              15 RUECW824-E-CFNALD-RURAL            PIC 9(003).
              15 RUECW824-E-CPRODT-RURAL            PIC 9(003).
              15 RUECW824-E-RESERVA                 PIC X(015).
           10 RUECW824-BLOCO-SAIDA.
              15 RUECW824-S-SUB-PROG-RURAL          PIC X(004).
