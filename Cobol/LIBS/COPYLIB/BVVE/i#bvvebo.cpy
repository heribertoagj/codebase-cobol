      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  I#BVVEBO - ARQUIVO DE DATAS INICIAL E FINAL DO CONTRATO       *
      *  TAMANHO: 80                                                   *
      *----------------------------------------------------------------*

       01  BVVEBO-REGISTRO.
           03 BVVEBO-CNPJ           PIC 9(09).
           03 BVVEBO-FILIAL         PIC 9(04).
           03 BVVEBO-CNPJ-DIGITO    PIC 9(02).
           03 BVVEBO-AGENCIA        PIC 9(05).
           03 BVVEBO-CONTA          PIC 9(13).
           03 BVVEBO-COD-PROP-ESQ   PIC X(03).
           03 BVVEBO-DT-INI         PIC 9(08).
           03 BVVEBO-DT-FIM         PIC 9(08).
           03 FILLER                PIC X(28).

      *    FORMATO DATAS: AAAAMMDD
