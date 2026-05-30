      *===============================================================*
      *  ARQUIVO COM LIQUIDACOES PARA RETORNO AO PAGFOR               *
      *            BOOK = I#BVVE96                TAMANHO = 160       *
      *===============================================================*
      *
       01 BVVE96-REGISTRO.
          05 BVVE96-PERFIL                   PIC  9(08).
          05 BVVE96-ASCG-ORIGEM              PIC  X(07).
          05 BVVE96-NUM-CTRL-ORIGINAL        PIC  X(20).
          05 BVVE96-TP-REG                   PIC  X(01).
          05 BVVE96-DETALHE.
             07 BVVE96-NUM-LIQUID               PIC  X(21).
             07 BVVE96-NIVEL-INFO               PIC  X(01).
             07 BVVE96-INFO-RETORNO             PIC  X(10).
             07 BVVE96-ERRO     REDEFINES       BVVE96-INFO-RETORNO.
                09 BVVE96-COD-ERRO              PIC  X(02) OCCURS 5.
             07 BVVE96-CNPJ-CPF                 PIC  9(09).
             07 BVVE96-FILIAL                   PIC  9(04).
             07 BVVE96-CTRL                     PIC  9(02).
             07 BVVE96-AGENCIA                  PIC  9(05).
             07 BVVE96-CONTA                    PIC  9(07).
             07 BVVE96-DATA                     PIC  9(08).
             07 BVVE96-VALOR                    PIC  9(13)V99.
             07 BVVE96-IND-CTRL                 PIC  X(01).
             07 BVVE96-NOME-CRED                PIC  X(30).
             07 BVVE96-BANDEIRA                 PIC  X(03).
             07 BVVE96-COD-CRED-PROD            PIC  9(04).
             07 BVVE96-FLAG-RET                 PIC  X(01).
             07 BVVE96-COD-BANDEIRA             PIC  9(03).
