      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMZ3 *
      * CRIACAO   : 11/2006                                   *
      * DESCRICAO : AREA PARA CHAMADA DO PROGRAMA CONE0453    *
      *             (RENOVACAO DE CONSULTAS NO CONE)          *
      *                                                       *
      * APLICACAO : MODULO DCOM5320 (INCLUSAO DOS DADOS       *
      *             BASICOS DA OPERACAO)                      *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************
       01  AREA-CONE0453.
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************
           05  ROTENT-CONE0453.
               10  ROTENT-CONE0453-CONSULTA        PIC  X(21).
               10  ROTENT-CONE0453-VALOR           PIC  X(17).
               10  ROTENT-CONE0453-VALOR-DISP      PIC  X(17).

      *********************************************************
      * AREA DE SAIDA                                         *
      *********************************************************
           05  ROTSAI-CONE0453.
               10  ROTSAI-CONE0453-CD-RETORNO      PIC  X(02).
               10  ROTSAI-CONE0453-CONS-ANTIGA     PIC  X(21).
               10  ROTSAI-CONE0453-CONS-NOVA       PIC  X(21).
               10  ROTSAI-CONE0453-MENSA           PIC  X(70).
               10  ROTSAI-CONE0453-ERRO-AREA.
                   15  ROTSAI-CONE0453-TIPO-ACESSO PIC  X(03).
                   15  ROTSAI-CONE0453-PGM         PIC  X(08).
                   15  ROTSAI-CONE0453-DBD-TAB     PIC  X(18).
                   15  ROTSAI-CONE0453-FUN-COMANDO PIC  X(10).
                   15  ROTSAI-CONE0453-SQL-CODE    PIC S9(09).
                   15  ROTSAI-CONE0453-LOCAL       PIC  X(04).
               10  ROTSAI-CONE0453-SQLCA           PIC  X(136).
