      *===============================================================*
      *   . NOME DA INC......I#ISDDPC                                 *
      *   . DESCRICAO........AREA DE COMUNICACAO COM O SBAT3000.      *
      *===============================================================*

       01  MCA-ISDPA.

           05  MPA-IDENTIFICACAO      PIC X(08)        VALUE 'ISDPA'.
           05  MPA-RCODE-API          PIC X(02)        VALUE ' '.
           05  MPA-RCODE-APLICACAO    PIC X(02)        VALUE ' '.
           05  MPA-DESTINO-MSG        PIC X(08)        VALUE ' '.
           05  MPA-TIPO-OPERACAO      PIC X(01)        VALUE ' '.
           05  MPA-CODIGO-REDE        PIC 9(07) COMP-3 VALUE 0.
           05  MPA-UNIDADE-LOGICA     PIC X(08)        VALUE ' '.
           05  MPA-NRO-SEQ-MSG        PIC 9(07) COMP-3 VALUE 0.
           05  MPA-CODG-PROPRIETARIO  PIC 9(15) COMP-3 VALUE 0.
           05  MPA-COMP-PROPRIETARIO  PIC 9(07) COMP-3 VALUE 0.
           05  MPA-CODIGO-USUARIO     PIC X(07)        VALUE ' '.
           05  MPA-VERSAO-APLICACAO   PIC 9(04) COMP   VALUE 0.
           05  MPA-IDENT-SISTEMA      PIC X(01)        VALUE ' '.
           05  MPA-NOME-PROGRAMA      PIC X(08)        VALUE ' '.
           05  MPA-ROTINA             PIC X(04)        VALUE ' '.
           05  MPA-OPCAO              PIC X(02)        VALUE ' '.
           05  MPA-TIPO-USUARIO       PIC X(01)        VALUE ' '.
           05  MPA-DADOS-USUARIO      PIC X(16)        VALUE ' '.
           05  MPA-USUARIO-INTERNO REDEFINES MPA-DADOS-USUARIO.
               10  MPA-AGEN-DEPTO     PIC 9(04).
               10  FILLER             PIC X(12).
           05  MPA-USUARIO-EXTERNO REDEFINES MPA-DADOS-USUARIO.
               10  MPA-NRO-CGC        PIC 9(09).
               10  MPA-NRO-FIL        PIC 9(05).
               10  MPA-NRO-CTL        PIC X(02).
           05  MPA-APLIC-REDE-LOCAL   PIC X(08).
           05  MPA-RESERVA            PIC X(30)        VALUE ' '.
