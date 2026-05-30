      *----------------------------------------------------------------*
      *  I#BVVEDM - AREA DE COMUNICACAO COM MODULO BCAR0000            *
      *  MODELO DE ENVIO FILA MQ.
      *----------------------------------------------------------------*

       01  WRK-COMMAREA-2450.
           03 WRK-2450-ENTRADA.
              05 WRK-2450-COD-MSG         PIC  X(09)    VALUE SPACES.
              05 WRK-2450-CNPJ            PIC  9(009)   VALUE ZEROS.
              05 WRK-2450-FILIAL          PIC  9(004)   VALUE ZEROS.
              05 WRK-2450-CTRL            PIC  9(002)   VALUE ZEROS.
              05 WRK-2450-TP-PSSOA        PIC  X(001)   VALUE SPACES.
              05 WRK-2450-RAIZ            PIC  X(001)   VALUE SPACES.
              05 WRK-2450-BANDEIRA        PIC  X(003)   VALUE SPACES.
              05 WRK-2450-DADOS-MQ.
                 07 WRK-2450-DADOS        PIC  X(300)   VALUE SPACES.
              05 WRK-2450-TAM-DADOS       PIC  9(003)   VALUE ZEROS.
           03 WRK-2450-SAIDA.
              05 WRK-2450-COD-RETORNO     PIC  9(02)     VALUE ZEROS.
              05 WRK-2450-NRO-CTRL-IF     PIC  X(23)     VALUE SPACES.
              05 WRK-2450-ERRO-CICS.
                 07 WRK-2450-EIBRESP       PIC  9(04)   VALUE ZEROS.
              05 WRK-2450-ERRO-DB2.
                 07 WRK-2450-ERR-DBD-TAB   PIC  X(18)     VALUE SPACES.
                 07 WRK-2450-ERR-FUN-COMANDO PIC  X(10)  VALUE SPACES.
                 07 WRK-2450-ERR-SQL-CODE    PIC S9(09)  VALUE 0.
                 07 WRK-2450-ERR-LOCAL       PIC  X(04)  VALUE SPACES.
              05 WRK-2450-ERRO-APL.
                 07 WRK-2450-ERR-MODULO      PIC  X(08)  VALUE SPACES.
                 07 WRK-2450-ERR-TEXTO       PIC  X(75)  VALUE SPACES.

