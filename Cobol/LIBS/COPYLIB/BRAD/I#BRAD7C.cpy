      *---------------------------------------------------------------* 00000010
      *               *** COPIA DA POL7100C ***                       * 00000020
      *---------------------------------------------------------------* 00000030
      *   PARAMETRO A SER PASSADO P/ A POOL7100 (MODULO TRATA/ ERRO)  * 00000040
      *                    (-INC POL7100C)                            * 00000050
      *---------------------------------------------------------------* 00000060
       01  ERRO-AREA.                                                   00000070
           05  ERR-TIPO-ACESSO        PIC  X(03).                       00000080
           05  ERR-PGM                PIC  X(08).                       00000090
           05  ERR-DADOS-IMS-DB2.                                       00000100
               07  ERR-DBD-TAB        PIC  X(18).                       00000110
               07  ERR-FUN-COMANDO    PIC  X(10).                       00000120
               07  ERR-STA-CODE       PIC  X(04).                       00000130
               07  ERR-SQL-CODE  REDEFINES                              00000140
                   ERR-STA-CODE       PIC S9(09) COMP-4.                00000150
               07  ERR-LOCAL          PIC  X(04).                       00000160
               07  ERR-SEGM           PIC  X(08).                       00000170
               07  FILLER             PIC  X(31).                       00000180
                                                                        00000190
           05  ERR-MQSERIES REDEFINES  ERR-DADOS-IMS-DB2.               00000200
               07  ERR-MQ-FILA        PIC  X(48).                       00000210
               07  ERR-MQ-FUNCAO      PIC  X(10).                       00000220
               07  ERR-MQ-RETURN-CODE PIC S9(09) COMP-4.                00000230
               07  ERR-MQ-REASON-CODE PIC S9(09) COMP-4.                00000240
               07  ERR-MQ-LOCAL       PIC  X(04).                       00000250
               07  FILLER             PIC  X(05).                       00000260
           05  ERR-TEXTO  REDEFINES  ERR-MQSERIES                       00000270
                                      PIC X(75).                        00000280
      *---------------------------------------------------------------* 00000290
      *  OS CPOS ABAIXO DEVEM SER INICIALIZADOS SO' QDO O APLICATIVO  * 00000300
      *  USAR O SISTEMA DE SENHAS (BRAD1000).                         * 00000310
      *---------------------------------------------------------------* 00000320
           05  ERR-DADOS-SENHAS.                                        00000330
               07  ERR-COD-USER       PIC  X(07).                       00000340
               07  ERR-COD-DEPTO      PIC  X(06).                       00000350
               07  ERR-MODULO         PIC  X(08)  VALUE SPACES.         00000360
