      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMZ4 *
      * CRIACAO   : 11/2006                                   *
      * DESCRICAO : AREA PARA CHAMADA DO PROGRAMA CONE0454    *
      *             (ABERTURA DE CONSULTAS NO CONE)           *
      *                                                       *
      * APLICACAO : MODULO DCOM5320 (INCLUSAO DOS DADOS       *
      *             BASICOS DA OPERACAO)                      *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************
       01  AREA-CONE0454.
      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

           05  ROTENT-CONE0454.
               10  ROTENT-CONE0454-CPF             PIC  9(09).
               10  ROTENT-CONE0454-FILIAL          PIC  9(04).
               10  ROTENT-CONE0454-CTR             PIC  9(02).
               10  ROTENT-CONE0454-AGE-SOLIC       PIC  9(05).
               10  ROTENT-CONE0454-AGE-PROP        PIC  9(05).
               10  ROTENT-CONE0454-CTA-PROP        PIC  9(07).
               10  ROTENT-CONE0454-CARTEIRA        PIC  X(03).
               10  ROTENT-CONE0454-GARNT           PIC  9(03).
               10  ROTENT-CONE0454-VALOR           PIC  9(15)V99.
               10  ROTENT-CONE0454-PRAZO           PIC  9(03).
               10  ROTENT-CONE0454-TP-PRAZO        PIC  X(01).
               10  ROTENT-CONE0454-ESPECIE         PIC  X(01).

      *********************************************************
      * AREA DE SAIDA                                         *
      *********************************************************
            05  ROTSAI-CONE0454-RECEBE.
               10  ROTSAI-CONE0454-CD-RETORNO      PIC  X(02).
               10  ROTSAI-CONE0454-CONSULTA.
                   15  ROTSAI-CONE0454-DANO-BASE   PIC  9(04).
                   15  ROTSAI-CONE0454-CJUNC-DEPDC PIC  9(05).
                   15  ROTSAI-CONE0454-CSEQ-STUDO  PIC  9(09).
               10  ROTSAI-CONE0454-DESPACHO        PIC  X(02).
               10  ROTSAI-CONE0454-ENQUADRA        PIC  X(01).
               10  ROTSAI-CONE0454-MENSAGEM        PIC  X(70).

               10  ROTSAI-CONE0454-ERRO-AREA.
                   15  ROTSAI-CONE0454-TIPO-ACESSO PIC  X(03).
                   15  ROTSAI-CONE0454-PGM         PIC  X(08).
                   15  ROTSAI-CONE0454-DBD-TAB     PIC  X(18).
                   15  ROTSAI-CONE0454-FUN-COMANDO PIC  X(10).
                   15  ROTSAI-CONE0454-SQL-CODE    PIC S9(09) COMP-4.
                   15  ROTSAI-CONE0454-LOCAL       PIC  X(04).
                   15  ROTSAI-CONE0454-SEGM        PIC  X(08).
               10  ROTSAI-CONE0454-SQLCA           PIC  X(136).
