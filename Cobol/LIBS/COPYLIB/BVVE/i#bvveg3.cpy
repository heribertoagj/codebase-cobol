      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CONTROLE DO ENVIO DE SOLICITACOES                   *
      *            DE TRAVA/DESTRAVA PARA A REDECARD                   *
      *  LRECL   : 023 (FB)                                            *
      *  NOME INC: I#BVVEG3                                            *
      *  DATA    : 20/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG3-REG.
           05  BVVEG3-ULTIMA-VERSAO           PIC 9(09).
           05  BVVEG3-DATA-ENVIO-ULTARQ       PIC 9(08).
           05  BVVEG3-HORA-ENVIO-ULTARQ       PIC 9(06).
      *----------------------------------------------------------------*
      * 001-009 ULTIMA VERSAO DE ARQUIVO ENVIADA/UTILIZADA
      * 010-017 DATA DE ENVIO DO ULTIMO ARQUIVO (AAAAMMDD)
      * 018-023 HORARIO DE ENVIO DO ULTIMO ARQUIVO (HHMMSS)
