      *---------------------------------------------------------------*
      * BOOK DO ARQUIVO RETORNO A VARIG DE DOCTOS REJEITADOS          *
      * TAMANHO : 0100                                                *
      * I#BVVEI3                          CRIACAO          : 12/95    *
      *---------------------------------------------------------------*

       01      B0I3-REGISTRO.
      *                                                         001 100
           05  B0I3-DT-RECEB                 PIC  9(008).
      *        FORMATO AAAAMMDD                                 001 008
           05  B0I3-NUM-VENDEDOR             PIC  9(013).
      *                                                         009 013
           05  B0I3-NUM-LOTE                 PIC  9(007).
      *                                                         022 007
           05  B0I3-NUM-ORPAG                PIC  9(007).
      *                                                         029 007
           05  B0I3-DT-COMPRA                PIC  9(008).
      *        FORMATO AAAAMMDD                                 036 008
           05  B0I3-VL-COMPR                 PIC  9(009)V99.
           05  B0I3-VL-SERV                  PIC  9(009)V99.
      *                                                         055 011
           05  B0I3-COD-MOT                  PIC  X(003).
      *                                                         066 003
           05  B0I3-OBR-FINANC               PIC  X(007).
      *                                                         069 007
           05  B0I3-CPR-VENDAS               PIC  X(015).
      *                                                         076 015
           05  B0I3-COD-ADM                  PIC  X(005).
      *                                                         091 005
           05  FILLER                        PIC  X(005).
      *                                                         096 005
