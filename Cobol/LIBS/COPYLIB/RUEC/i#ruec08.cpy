
     *** BOOK DE COMUNICACAO ENTRE MODULOS E PGM'S DE ALTERACAO ***

     *** RUEC8500 - MODULO DE CONTROLE DE PASSOS       (MCP)    ***
     *** RUEC8510 - MODULO DE TRATAMENTO DE MENSAGENS  (MTM)    ***
     *** RUEC8520 - MODULO DE CRIACAO DE CONTRATO      (MCC)    ***

       01  LK8-AREA-NAVEGA.

           03  LK8-AREA-ENTRADA.

               05  LK8-CODIGO-AGENCIA     PIC 9(05).
               05  LK8-CODIGO-CEDULA      PIC 9(09).
               05  LK8-CODIGO-TERMINAL    PIC X(08).
               05  LK8-NUMORD-CHAMADOR    PIC 9(02).

           03  LK8-AREA-SAIDA.

               05  LK8-STATUS-RETORNO     PIC X(02).
     ***       "  " ====> QUANDO TUDO OCORREU CONFORME PLANEJADO
     ***       "DB" ====> QUANDO OCORREU ALGUM ERRO NO DB2
     ***       "BL" ====> QUANDO ALGO "BLOQUEADO" FOI SELECIONADO

               05  LK8-MENSAGEM           PIC X(79).

               05  LK8-CODIGO-PROXPGM     PIC X(08).

