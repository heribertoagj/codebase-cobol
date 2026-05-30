      ***===========================================================***
      *** NOME INC                                     LENGTH=00259 ***
      *** I#SLIG20                                                  ***
      ***-----------------------------------------------------------***
      ***                                                           ***
      *** DESCRICAO: IMAGEM DA TABELA DE NEGOCIACOES CBCL +         ***
      ***            CAMPOS IMAGEM CLIE COBR                        ***
      ***                                                           ***
      ***===========================================================***

       01  REG-NEGOCIAC-CBCL-C.
           05  SLIG20-CHAVE.
              10  SLIG20-CBCO-PRODT-COBR     PIC S9(03)V USAGE COMP-3.
              10  SLIG20-CIDTFD-PRODT-COBR   PIC S9(02)V USAGE COMP-3.
              10  SLIG20-CNEGOC-COBR         PIC S9(18)V USAGE COMP-3.
           05  SLIG20-CCLI-COBR              PIC S9(09)V USAGE COMP-3.
           05  SLIG20-CENDER-CLI-COBR        PIC S9(05)V USAGE COMP-3.
           05  SLIG20-CEMPR-TROCA-FITA       PIC S9(18)V USAGE COMP-3.
           05  SLIG20-CAG-BCRIA              PIC S9(05)V USAGE COMP-3.
           05  SLIG20-NULO-CAG-BCRIA         PIC  X(01).
           05  SLIG20-CTPO-CART-COBR         PIC S9(02)V USAGE COMP-3.
           05  SLIG20-DPRIM-NEGOC-CLI        PIC  X(10).
           05  SLIG20-DULT-CREDT-CLI         PIC  X(10).
           05  SLIG20-NULO-DULT-CREDT-CLI    PIC  X(01).
           05  SLIG20-DULT-REG-TITLO         PIC  X(10).
           05  SLIG20-NULO-DULT-REG-TITLO    PIC  X(01).
           05  SLIG20-CNRO-RELTV-PG          PIC S9(18)V USAGE COMP-3.
           05  SLIG20-NULO-CNRO-RELTV-PG     PIC  X(01).
           05  SLIG20-DULT-ALT-NEGOC         PIC  X(10).
           05  SLIG20-NULO-DULT-ALT-NEGOC    PIC  X(01).
           05  SLIG20-CSIT-NEGOC-COBR        PIC S9(02)V USAGE COMP-3.
           05  SLIG20-CINDCD-NEGOC-ESPCL     PIC  X(01).
           05  SLIG20-ICLI-COBR              PIC  X(40).
           05  SLIG20-CSIT-ANTER-NEGOC       PIC S9(02)V USAGE COMP-3.
           05  SLIG20-DULT-BAIXA-TITLO       PIC  X(10).
           05  SLIG20-NULO-DULT-BAIXA-TITLO  PIC  X(01).
           05  SLIG20-CORIGE-COBR            PIC S9(03)V USAGE COMP-3.
           05  SLIG20-CNRO-SERIE-SOFTW       PIC  X(15).
           05  SLIG20-NULO-CNRO-SERIE-SOFTW  PIC  X(01).
           05  SLIG20-NEGOC-CGC-CPF-ST.
.           07  SLIG20-NEGOC-CGC-CPF         PIC 9(009).
           05  SLIG20-NEGOC-FIL-CGC-CPF-ST.
.           07  SLIG20-NEGOC-FIL-CGC-CPF     PIC 9(005).
           05  SLIG20-NEGOC-CTR-CGC-CPF      PIC 9(002).
           05  SLIG20-NEGOC-ICLI-COBR        PIC X(040).
           05  SLIG20-NEGOC-CGRP-CLI-COBR    PIC 9(009).
           05  SLIG20-NEGOC-CTPO-PSSOA       PIC X(001).
           05  SLIG20-NEGOC-CAG-BCRIA        PIC 9(005).
           05  SLIG20-NEGOC-CBCO             PIC 9(003).
           05  SLIG20-FILLER                 PIC X(019).
