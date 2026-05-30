      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMX1 *
      * CRIACAO   : 01/2006                                   *
      * DESCRICAO : AREA PARA INCLUSAO DO LOG COMPLETO E      *
      *             RESUMIDO                                  *
      * APLICACAO : MODULO - DCOM5075                         *
      *                                                       *
      * TAMANHO   : ENTRADA = LRECL - 3104                    *
      *             SAIDA   = LRECL - 0084                    *
      *********************************************************
       01  X1-REGISTRO.
           03 X1-ARGUMENTOS-ENTRADA.
             05  X1-CELMTO-DESC-COML    PIC S9(0003) COMP-3.
             05  X1-CEVNTO-LOG-DESC     PIC S9(0003) COMP-3.
             05  X1-HULT-ATULZ          PIC  X(0026).
             05  X1-CINDCD-MANUT-REG    PIC  X(0001).
             05  X1-CCHAVE-ELMTO-DESC   PIC  X(0045).
             05  X1-WREG-LOG-ANTES.
              07 X1-WREG-LOG-ANTES-LEN  PIC S9(4) COMP.
              07 X1-WREG-LOG-ANTES-TEXT PIC X(1500).
             05  X1-WREG-LOG-APOS.
              07 X1-WREG-LOG-APOS-LEN  PIC S9(4) COMP.
              07 X1-WREG-LOG-APOS-TEXT PIC X(1500).
             05  X1-CTRANS-PROG-ATULZ   PIC  X(0008).
             05  X1-CINDCD-MODLD-PROCM PIC  X(0001).
             05  X1-CEMPR-INC          PIC  9(0005) COMP-3.
             05  X1-CDEPDC             PIC  9(0005) COMP-3.
             05  X1-CFUNC-BDSCO        PIC  9(0009) COMP-3.
             05  X1-CTERM              PIC  X(0008).
           03 X1-RETORNO.
             05  X1-COD-RETORNO        PIC  9(0004).
             05  X1-MENSAGEM           PIC  X(0079).
