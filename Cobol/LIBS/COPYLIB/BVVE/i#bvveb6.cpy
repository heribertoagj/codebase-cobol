      *===============================================================*
      *  ARQUIVO COM LIQUIDACOES PARA RETORNO AO PAGFOR               *
      *            BOOK = I#BVVEB6                TAMANHO = 260       *
      *===============================================================*
      *
       01 BVVEB6-REGISTRO.
          05 BVVEB6-PERFIL                   PIC  9(08).
          05 BVVEB6-ASCG-ORIGEM              PIC  X(07).
          05 BVVEB6-NUM-CTRL-ORIGINAL        PIC  X(20).
          05 BVVEB6-TP-REG                   PIC  X(01).
          05 BVVEB6-HEADER.
             07 BVVEB6-HE-NOME-EMPR          PIC  X(40).
             07 BVVEB6-HE-TIP-SERVICO        PIC  9(02).
             07 BVVEB6-HE-ORIGEM-ARQ         PIC  9(01).
             07 BVVEB6-HE-NUM-REMESSA        PIC  9(05).
             07 BVVEB6-HE-NUM-RETORNO        PIC  9(05).
             07 BVVEB6-HE-DT-GRAV-ARQ        PIC  9(08).
             07 BVVEB6-HE-HR-GRAV-ARQ        PIC  9(06).
             07 BVVEB6-HE-TIP-PROCESS        PIC  9(01).
             07 FILLER                       PIC  X(156).
          05 BVVEB6-DETALHE    REDEFINES   BVVEB6-HEADER.
             07 BVVEB6-NUM-LIQUID               PIC  X(21).
             07 BVVEB6-NIVEL-INFO               PIC  X(01).
             07 BVVEB6-INFO-RETORNO             PIC  X(10).
             07 BVVEB6-ERRO     REDEFINES       BVVEB6-INFO-RETORNO.
                09 BVVEB6-COD-ERRO              PIC  X(02) OCCURS 5.
             07 BVVEB6-CNPJ-CPF                 PIC  9(09).
             07 BVVEB6-FILIAL                   PIC  9(04).
             07 BVVEB6-CTRL                     PIC  9(02).
             07 BVVEB6-AGENCIA                  PIC  9(05).
             07 BVVEB6-CONTA                    PIC  9(07).
             07 BVVEB6-DATA                     PIC  9(08).
             07 BVVEB6-VALOR                    PIC  9(13)V99.
             07 BVVEB6-ISPB-DEV-CRED            PIC  9(08).
             07 BVVEB6-ISPB-DEV-CRED-X          REDEFINES
                BVVEB6-ISPB-DEV-CRED            PIC  X(08).
             07 BVVEB6-COD-PROD                 PIC  X(03).
             07 BVVEB6-DEB-CRED                 PIC  X(01).
             07 BVVEB6-IDTF-TRANSF              PIC  9(01).
             07 BVVEB6-IDENT-PART-PRINC         PIC  X(08).
             07 BVVEB6-IDENT-PART-ADM           PIC  X(08).
             07 FILLER                          PIC  X(104).

