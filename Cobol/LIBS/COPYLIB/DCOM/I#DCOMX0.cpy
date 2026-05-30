      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMX0 *
      * CRIACAO   : 01/2006                                   *
      * DESCRICAO : AREA PARA CONSULTA DO LOG COMPLETO E      *
      *             RESUMIDO                                  *
      * APLICACAO : MODULO - DCOM4045                         *
      *                                                       *
      * TAMANHO   : ENTRADA = LRECL - 0051                    *
      *             SAIDA   = LRECL - 0309                    *
      *********************************************************
       01  X0-REGISTRO.
           03 X0-ARGUMENTOS-ENTRADA.
             05  X0-CELMTO-DESC-COML   PIC S9(0003) COMP-3.
             05  X0-CEVNTO-DESC-COML   PIC S9(0003) COMP-3.
             05  X0-CCHAVE-ELMTO-DESC  PIC  X(0045).
           03 X0-RETORNO.
             05  X0-COD-RETORNO        PIC  9(0004).
             05  X0-MENSAGEM           PIC  X(0079).
             05  X0-HULT-ATULZ-INC     PIC  X(0026).
             05  X0-CFUNC-BDSCO-INC    PIC  9(0009).
             05  X0-IFUNC-BDSCO-INC    PIC  X(0040).
             05  X0-HULT-ATULZ-ALT     PIC  X(0026).
             05  X0-CFUNC-BDSCO-ALT    PIC  9(0009).
             05  X0-IFUNC-BDSCO-ALT    PIC  X(0040).
             05  X0-HULT-ATULZ-EXC     PIC  X(0026).
             05  X0-CFUNC-BDSCO-EXC    PIC  9(0009).
             05  X0-IFUNC-BDSCO-EXC    PIC  X(0040).
