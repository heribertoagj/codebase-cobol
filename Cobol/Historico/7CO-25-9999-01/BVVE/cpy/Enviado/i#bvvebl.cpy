      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  I#BVVEBL - ARQUIVO DE ANTECIPACOES DE ORPAG DCOM + BANDEIRA   *
      *  TAMANHO: 160                                                  *
      *----------------------------------------------------------------*

       01  BVVEBL-REGISTRO.
7C2511*    03 BVVEBL-CNPJ           PIC 9(09).
7C2511     03 BVVEBL-CNPJ           PIC X(09).
7C2511*    03 BVVEBL-FILIAL         PIC 9(04).
7C2511     03 BVVEBL-FILIAL         PIC X(04).
           03 BVVEBL-CNPJ-DIGITO    PIC 9(02).
           03 BVVEBL-AGENCIA        PIC 9(05).
           03 BVVEBL-CONTA          PIC 9(13).
           03 BVVEBL-PRODUTO        PIC 9(05).
           03 BVVEBL-DT-VCTO-PARC   PIC 9(08).
           03 BVVEBL-VLR-DESCONTO   PIC 9(15)V99 COMP-3.
           03 BVVEBL-DT-INI-CONTR   PIC 9(08).
           03 BVVEBL-BANDEIRA       PIC X(15).
           03 BVVEBL-CREDENC        PIC X(10).
           03 BVVEBL-COD-BAND       PIC 9(01).
           03 FILLER                PIC X(71).
