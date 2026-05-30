      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CONTROLE DO ENVIO DE SOLICITACOES DE TRANSFERENCIA  *
      *            DE DOMICILIO BANCARIO DE ESTABELECIMENTOS  PARA  A  *
      *            REDECARD                                            *
      *  LRECL   : 023 (FB)                                            *
      *  NOME INC: I#BVVEGU                                            *
      *  DATA    : 25/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGU-REG.
           05  BVVEGU-ULTVERSAO-ENVIADA       PIC 9(09).
      * 001-009 ULTIMA VERSAO DE ARQUIVO ENVIADO 'A REDECARD
           05  BVVEGU-DATA-ULTENVIO           PIC 9(08).
      * 010-017 DATA DO ULTIMO ENVIO DE ARQUIVO 'A REDECARD
           05  BVVEGU-HORA-ULTENVIO           PIC 9(06).
      * 018-023 HORARIO DO ULTIMO ENVIO DE ARQUIVO 'A REDECARD
      *----------------------------------------------------------------*
