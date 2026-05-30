       01  JC-ARGUMENTOS-ENTRADA.
       05  JC-COLUNAS-DA-TABELA.
           10  JC-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  JC-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  JC-NPCELA-DESC-COML             PIC S9(5)V COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  JC-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  JC-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  JC-RETORNO.
       05  JC-COD-RETORNO                         PIC 9(04).
       05  JC-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  JC-PROXIMO-RESTART                     PIC 9(05).
       05  JC-QTDE-RETORNADA                      PIC 9(03).
       05  JC-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  JC-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJC.
           10  JCRT-NPCELA-DESC-COML           PIC S9(5)V COMP-3.
           10  JCRT-IPSSOA-SACDO               PIC X(60).
           10  JCRT-DVCTO-PCELA-DESC           PIC X(10).
           10  JCRT-VPRINC-PCELA-DESC          PIC S9(15)V9(2) COMP-3.
           10  JCRT-CSEU-NRO-TITLO             PIC X(15).
           10  JCRT-RANLSE-CREDT-CLI           PIC X(20).
