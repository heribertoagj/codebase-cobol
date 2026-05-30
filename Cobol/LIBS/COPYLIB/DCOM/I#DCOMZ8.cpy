      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMZ8 *
      * CRIACAO   : 11/2006                                   *
      * DESCRICAO : AREA PARA CHAMADA DO PROGRAMA DCOM5372    *
      *             (SENSIBILIZA CONTRATO/CONVENIO)           *
      *                                                       *
      * APLICACAO : MODULO DCOM5320 (INCLUSAO DOS DADOS       *
      *             BASICOS DA OPERACAO)                      *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01     ENTRADA-5372-ROTEADOR.
              03     ROTENT-5372-COD-RETORNO        PIC  X(04).
              03     ROTENT-5372-MSG-RETORNO        PIC  X(79).
              03     ROTENT-5372-RESTART            PIC  9(05).
              03     ROTENT-5372-FLAG               PIC  X(01).
              03     ROTENT-5372-CFUNC-BDSCO        PIC  9(09).
              03     ROTENT-5372-CTERM              PIC  X(08).
              03     ROTENT-5372-CCONTR-LIM-DESC    PIC  9(09).
              03     ROTENT-5372-CIND-SOMA-SUBTRAI  PIC  9(01).
      *                             1-SOMA E 2-SUBTRAI
              03     ROTENT-5372-VDIFER-OPER-DESC   PIC  9(15)V99.

      *********************************************************
      * AREA DE SAIDA                                         *
      *********************************************************
       01     SAIDA-5372-ROTEADOR.
              03     ROTSAI-5372-HEADER.
                05   ROTSAI-5372-COD-RETORNO        PIC  X(04).
                05   ROTSAI-5372-MSG-RETORNO        PIC  X(79).
                05   ROTSAI-5372-RESTART            PIC  9(05).
                05   ROTSAI-5372-FLAG               PIC  X(01).
                05   ROTSAI-5372-COUNT              PIC  9(09).
              03     ROTSAI-5372-CONSISTENCIA.
                05   ROTSAI-5372-TAMANHO-ERRO       PIC  9(01).
                05   ROTSAI-5372-TABELA-ERROS       OCCURS 03  TIMES.
                  07 ROTSAI-5372-CAMPOS-ERRO        PIC  9(01).
