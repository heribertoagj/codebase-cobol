      *===============================================================*
      *  ARQUIVO COM LIQUIDACOES PARA RETORNO AO PAGFOR               *
      *            BOOK = I#BVVEA6                TAMANHO = 160       *
      *===============================================================*
      *
       01 BVVEA6-REGISTRO.
          05 BVVEA6-PERFIL                   PIC  9(08).
          05 BVVEA6-ASCG-ORIGEM              PIC  X(07).
          05 BVVEA6-NUM-CTRL-ORIGINAL        PIC  X(20).
          05 BVVEA6-TP-REG                   PIC  X(01).
          05 BVVEA6-HEADER.
             07 BVVEA6-HE-NOME-EMPR          PIC  X(40).
             07 BVVEA6-HE-TIP-SERVICO        PIC  9(02).
             07 BVVEA6-HE-ORIGEM-ARQ         PIC  9(01).
             07 BVVEA6-HE-NUM-REMESSA        PIC  9(05).
             07 BVVEA6-HE-NUM-RETORNO        PIC  9(05).
             07 BVVEA6-HE-DT-GRAV-ARQ        PIC  9(08).
             07 BVVEA6-HE-HR-GRAV-ARQ        PIC  9(06).
             07 BVVEA6-HE-TIP-PROCESS        PIC  9(01).
             07 FILLER                       PIC  X(56).
          05 BVVEA6-DETALHE    REDEFINES   BVVEA6-HEADER.
             07 BVVEA6-NUM-LIQUID               PIC  X(21).
             07 BVVEA6-NIVEL-INFO               PIC  X(01).
             07 BVVEA6-INFO-RETORNO             PIC  X(10).
             07 BVVEA6-ERRO     REDEFINES       BVVEA6-INFO-RETORNO.
                09 BVVEA6-COD-ERRO              PIC  X(02) OCCURS 5.
             07 BVVEA6-CNPJ-CPF                 PIC  9(09).
             07 BVVEA6-FILIAL                   PIC  9(04).
             07 BVVEA6-CTRL                     PIC  9(02).
             07 BVVEA6-AGENCIA                  PIC  9(05).
             07 BVVEA6-CONTA                    PIC  9(07).
             07 BVVEA6-DATA                     PIC  9(08).
             07 BVVEA6-VALOR                    PIC  9(13)V99.
             07 FILLER                          PIC  X(42).

