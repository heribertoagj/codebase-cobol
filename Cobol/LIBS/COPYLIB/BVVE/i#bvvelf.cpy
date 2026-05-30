      *----------------------------------------------------------------*
      *  I#BVVELF     CLIENTES PARA AUTORIZACAO                        *
      *  ARQUIVO                                                       *
      *----------------------------------------------------------------*
       01  BVVELF-REGISTRO.
           03  BVVELF-AGEN               PIC S9(05)    VALUE +0  COMP-3.
           03  BVVELF-CONTA              PIC S9(07)    VALUE +0  COMP-3.
           03  BVVELF-PRODUTO            PIC S9(05)    VALUE +0  COMP-3.
           03  BVVELF-TPMOEDA            PIC S9(05)    VALUE +0  COMP-3.
           03  BVVELF-DTMOVTO.
               05  BVVELF-DTMOVTO-DD     PIC  9(02)    VALUE 0.
               05  FILLER                PIC  X(01)    VALUE '.'.
               05  BVVELF-DTMOVTO-MM     PIC  9(02)    VALUE 0.
               05  FILLER                PIC  X(01)    VALUE '.'.
               05  BVVELF-DTMOVTO-SS     PIC  9(02)    VALUE 0.
               05  BVVELF-DTMOVTO-AA     PIC  9(02)    VALUE 0.
           03  BVVELF-VLRCRED            PIC S9(13)V99 VALUE +0  COMP-3.
           03  BVVELF-VLRDEB             PIC S9(13)V99 VALUE +0  COMP-3.
           03  BVVELF-TP-PSSOA           PIC X(1).
           03  BVVELF-CPF-CNPJ           PIC 9(14).
           03  BVVELE-CPF                REDEFINES
               BVVELF-CPF-CNPJ.
               05  BVVELF-CPF-NUM        PIC 9(011).
               05  BVVELF-ZEROS          PIC 9(003).
           03  BVVELF-CREDENCIADORA      PIC 9(04).
           03  BVVELF-BANDEIRA           PIC 9(03).
           03  BVVELF-FORMA-PGTO         PIC 9(03).
