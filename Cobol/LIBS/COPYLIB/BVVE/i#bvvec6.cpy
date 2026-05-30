      *===============================================================*
      *  ARQUIVO COM LIQUIDACOES PARA RETORNO AO PAGFOR               *
      *            BOOK = I#BVVEC6                TAMANHO = 160       *
      *===============================================================*
      *
       01 BVVEC6-REGISTRO.
          05 BVVEC6-PERFIL                   PIC  9(08).
          05 BVVEC6-ASCG-ORIGEM              PIC  X(07).
          05 BVVEC6-NUM-CTRL-ORIGINAL        PIC  X(20).
          05 BVVEC6-TP-REG                   PIC  X(01).
          05 BVVEC6-HEADER.
             07 BVVEC6-HE-NOME-EMPR          PIC  X(40).
             07 BVVEC6-HE-TIP-SERVICO        PIC  9(02).
             07 BVVEC6-HE-ORIGEM-ARQ         PIC  9(01).
             07 BVVEC6-HE-NUM-REMESSA        PIC  9(05).
             07 BVVEC6-HE-NUM-RETORNO        PIC  9(05).
             07 BVVEC6-HE-DT-GRAV-ARQ        PIC  9(08).
             07 BVVEC6-HE-HR-GRAV-ARQ        PIC  9(06).
             07 BVVEC6-HE-TIP-PROCESS        PIC  9(01).
             07 FILLER                       PIC  X(56).
          05 BVVEC6-DETALHE    REDEFINES   BVVEC6-HEADER.
             07 BVVEC6-NUM-LIQUID               PIC  X(21).
             07 BVVEC6-NIVEL-INFO               PIC  X(01).
             07 BVVEC6-INFO-RETORNO             PIC  X(10).
             07 BVVEC6-ERRO     REDEFINES       BVVEC6-INFO-RETORNO.
                09 BVVEC6-COD-ERRO              PIC  X(02) OCCURS 5.
7C2511*      07 BVVEC6-CNPJ-CPF                 PIC  9(09).
7C2511       07 BVVEC6-CNPJ-CPF                 PIC  X(09).
7C2511*      07 BVVEC6-FILIAL                   PIC  9(04).
7C2511       07 BVVEC6-FILIAL                   PIC  X(04).
             07 BVVEC6-CTRL                     PIC  9(02).
             07 BVVEC6-AGENCIA                  PIC  9(05).
             07 BVVEC6-CONTA                    PIC  9(07).
             07 BVVEC6-DATA                     PIC  9(08).
             07 BVVEC6-VALOR                    PIC  9(13)V99.
             07 BVVEC6-IND-CTRL                 PIC  X(01).
             07 BVVEC6-NOME-CRED                PIC  X(30).
SVK          07 BVVEC6-BANDEIRA                 PIC  X(03).
SVK1         07 BVVEC6-COD-CRED-PROD            PIC  9(04).
IM1          07 BVVEC6-FLAG-RET                 PIC  X(01).
SVK1         07 FILLER                          PIC  X(03).
