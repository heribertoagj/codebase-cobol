      *---------------------------------------------------------------*
      * BOOK DO ARQUIVO RETORNO A VARIG DE DOCTOS CREDITADOS          *
      * TAMANHO : 0100                                                *
      * I#BVVEI4                          CRIACAO          : 12/95    *
      *---------------------------------------------------------------*

       01      B0I4-REGISTRO.
      *                                                         001 100
           05  B0I4-DT-RECEB                 PIC  9(008).
      *        FORMATO AAAAMMDD                                 001 008
           05  B0I4-DT-CREDIT                PIC  9(008).
      *        FORMATO AAAAMMDD                                 009 008
           05  B0I4-NUM-VENDEDOR             PIC  9(013).
      *                                                         017 013
           05  B0I4-NUM-LOTE                 PIC  9(007).
      *                                                         030 007
           05  B0I4-VL-LOTE                  PIC  9(011)V99.
      *                                                         037 013
           05  B0I4-VL-COMIS                 PIC  9(009)V99.
           05  B0I4-VL-LIQUIDO               PIC  9(011)V99.
      *                                                         061 013
           05  FILLER                        PIC  X(027).
      *                                                         074 027
