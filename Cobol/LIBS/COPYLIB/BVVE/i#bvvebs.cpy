      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  I#BVVEBS - ARQUIVO DE ANTECIPACOES DE ORPAG DCOM              *
      *  TAMANHO: 100                                                  *
      *----------------------------------------------------------------*

       01  BVVEBS-REGISTRO.
           03 BVVEBS-CNPJ           PIC 9(09).
           03 BVVEBS-FILIAL         PIC 9(04).
           03 BVVEBS-CNPJ-DIGITO    PIC 9(02).
           03 BVVEBS-AGENCIA        PIC 9(05).
           03 BVVEBS-CONTA          PIC 9(13).
           03 BVVEBS-PRODUTO        PIC 9(05).
           03 BVVEBS-DT-OPER        PIC X(10).
           05 BVVEBS-VLR-DESCONTO   PIC 9(15)V99 COMP-3.
           03 BVVEBS-DT-INI-CONTR   PIC X(10).
           05 FILLER                PIC X(13).

