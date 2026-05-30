      ***===========================================================***
      *** NOME INC                                     LENGTH=00160 ***
      *** I#SLIG16                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG          ***
      ***            ARQUIVO DE INDICES DOS CLIENTES SELECIONADOS   ***
      ***            PARA EMISSAO DE RELATORIOS                     ***
      ***                                                           ***
      ***===========================================================***
      *
       01  REG-INDICES.
           03 SLIG16-CLIENTE-CARTEIRA.
              05 SLIG16-CGC-CPF-CLIENTE         PIC 9(009)     COMP-3.
              05 SLIG16-TP-PESS                 PIC X(001).
              05 SLIG16-TIPO-CART               PIC 9(001).
              05 SLIG16-NOME-CLIENTE            PIC X(040).
           03 SLIG16-PERIODO.
              05 SLIG16-PER-INI                 PIC 9(009)     COMP-3.
              05 SLIG16-PER-FIM                 PIC 9(009)     COMP-3.
           03 SLIG16-INDICE-TIT.
              05 SLIG16-INDC-TIT-LIQDZ          PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-VCTO-CART      PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-VCTO-ANTER     PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-BAIXA-ANTEC    PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-LIQ-BAIXA      PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-PRORROGA       PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-ABATIMEN       PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-TIT-PGTO-ANTEC     PIC S9(005)V99 COMP-3.
           03 SLIG16-INDICE-CHQ.
              05 SLIG16-INDC-CHQ-LIQDZ-FINAL    PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-LIQDZ-PRIM-DEP PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-LIQDZ-REAPR    PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-BAIXA-ANTEC    PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-VCTO-PER       PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-DEPOS          PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-REAPRES        PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-CHQ-DEVOL-ANTER    PIC S9(005)V99 COMP-3.
           03 SLIG16-INDICE-VND.
              05 SLIG16-INDC-VND-LIQDZ-TOTAL    PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-VND-LIQDZ-COMPR    PIC S9(005)V99 COMP-3.
              05 SLIG16-INDC-VND-LIQDZ-FORNEC   PIC S9(005)V99 COMP-3.
      * === CAMPOS DUPLICADOS PARA STRING ===
           03 SLIG16-CGC-CPF-CLIENTE-ST         PIC  X(009).
           03 FILLER                            PIC  X(018).
