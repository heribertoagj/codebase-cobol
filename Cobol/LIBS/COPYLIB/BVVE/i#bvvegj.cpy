      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CONTROLE DO ENVIO DE AFILIACOES DE                  *
      *            ESTABELECIMENTOS PARA A REDECARD                    *
      *  LRECL   : 037 (FB)                                            *
      *  NOME INC: I#BVVEGJ                                            *
      *  DATA    : 21/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGJ-REG.
           05  BVVEGJ-ULTLOTE-ENVIADO         PIC 9(06).
           05  BVVEGJ-DTENVIO-ULTLOTE         PIC 9(08).
           05  BVVEGJ-HRENVIO-ULTLOTE         PIC 9(06).
           05  BVVEGJ-ULT-NSU                 PIC 9(10).
           05  BVVEGJ-QTDEREG-LOTE            PIC 9(07).
      *----------------------------------------------------------------*
      * 001-006 NUMERO DO ULTIMO LOTE ENVIADO PARA A REDECARD
      * 007-014 DATA DE ENVIO DO ULTIMO LOTE PARA A REDECARD AAAAMMDD
      * 015-020 HORARIO DE ENVIO DO ULTIMO LOTE PARA A REDECARD HHMMSS
      * 021-030 ULTIMO NUMERO SEQUENCIAL UNICO (NSU) UTILIZADO
      *----------------------------------------------------------------*
