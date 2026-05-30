*********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMX3 *
      * CRIACAO   : 06/2006                                   *
      * DESCRICAO : SELECAO DE PESSOAS CADASTRADAS            *
      *             NO CADU ATRAVES DE AGENCIA E CONTA OU     *
      *             CNPJ / CPF. OBTEM INFORMACOES COMPLETAS   *
      *             DO CLIENTE                                *
      *                                                       *
      *           - SE FOR INFORMADO UM CPF/CNPJ, SER�O       *
      *             RETORNADOS OS DADOS DAQUELA PESSOA        *
      *             NA PRIMEIRA OCORRENCIA DA �REA DE RETORNO,*
      *             E APENAS ESTA OCORRENCIA ESTAR� PREENCHIDA*
      *                                                       *
      *           - SE FOREM INFORMADAS AGENCIA E CONTA,      *
      *             SER�O RETORNADOS OS DADOS DOS RESPECTIVOS *
      *             TITULARES, UM EM CADA OCORR�NCIA DA �REA  *
      *             DE RETORNO.                               *
      *                                                       *
      * APLICACAO : MODULO DCOM6408                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * �REA DE ENTRADA                                       *
      *********************************************************

       01  ROTENT-6408-AREA.
           03 ROTENT-6408-ENVIO.
              05 ROTENT-6408-COD-ENV            PIC  X(04).
      *      INFORMAR ZEROS
              05 ROTENT-6408-MSG-ENV            PIC  X(78).
      *      INFORMAR BRANCOS
              05 ROTENT-6408-TITULAR-ENV        PIC  X(01).
      *      INFORMAR 'S' SE SOMENTE TITULAR OU 'N'
              05 ROTENT-6408-REST-ENV           PIC  9(05).
      *      INFORMAR NUMERO DE REGISTROS PARA RESTART
              05 ROTENT-6408-FLAG-ENV           PIC  X(01).
      *      INFORMAR N
              05 ROTENT-6408-START-ENV          PIC  X(01).
      *      INFORMAR N NA PRIMEIRA CHAMADA E S A PARTIR DA SEGUNDA
              05 ROTENT-6408-CFUNC-ENV          PIC  9(09).
      *      INFORMAR CODIGO DO FUNCIONARIO
              05 ROTENT-6408-CTERM-ENV          PIC  X(08).
      *      INFORMAR TERMINAL
              05 ROTENT-6408-CJUNC-ENV          PIC  9(05).
              05 ROTENT-6408-CTAC-ENV.
                 07 ROTENT-6408-CCTAC-ENV       PIC  9(07).
                 07 ROTENT-6408-DCTAC-ENV       PIC  9(01).
      * FS2511: Migrado de PIC 9(09) para PIC X(09) para permitir tratamento alfanum�rico/zeros � esquerda
4S2511* ANTES: ROTENT-6408-NUM-CNPJ-ENV       PIC  9(09).
4S2511            05 ROTENT-6408-NUM-CNPJ-ENV       PIC  X(09).
      * FS2511: Migrado de PIC 9(04) para PIC X(04) para permitir tratamento alfanum�rico/zeros � esquerda
4S2511* ANTES: ROTENT-6408-FIL-CNPJ-ENV       PIC  9(04).
4S2511            05 ROTENT-6408-FIL-CNPJ-ENV       PIC  X(04).
              05 ROTENT-6408-CTR-CNPJ-ENV       PIC  9(02).
              05 ROTENT-6408-CCLUB-ENV          PIC  9(10).

      *********************************************************
      *  AREA DE RETORNO                                      *
      *********************************************************

      *    03 ROTSAI-6408-RETORNO.
       01 ROTSAI-6408-RETORNO.
              05 ROTSAI-6408-COD-RET            PIC  X(04).
      *
      *            00 - REGISTRO ENCONTRADO
      *            02 - RESTART
      *            03 - NENHUM REGISTRO ENCONTRADO NO CADU
      *            99 - PROBLEMAS DB2
      *           100  DADOS INCONSISTENTES
      *
              05 ROTSAI-6408-MENS-RET           PIC  X(79).
      *      RETORNAR� MENSAGEM QUANDO NECESSARIO
              05 ROTSAI-6408-REST-RET           PIC  9(05).
      *      RETORNARA TOTAL DE REGISTRO LIDOS
              05 ROTSAI-6408-FLAG-RET           PIC  X(01).
      *      RETORNARA S QWUANDO FIM DE ARQUIVO
              05 ROTSAI-6408-QTDE-RET           PIC  9(03).
      *      RETORNARA QUANDIDADE DE REGISTROS NA LISTA
              05 ROTSAI-6408-COUNT-RET          PIC  9(09).
      *      RETORNAR� ZEROS (ITULIZADO PARA TOTAIS)

      *
              05 FILLER       OCCURS      10  TIMES.
                 07 ROTSAI-6408-NM-RZSOC-RET    PIC  X(70).
                 07 ROTSAI-6408-CCLUB-RET       PIC  9(10).
                 07 ROTSAI-6408-CNPJ-CADU       PIC  X(15).
                 07 ROTSAI-6408-TP-PESSOA       PIC  X(01).
                 07 ROTSAI-6408-TIT-CLI-RET     PIC  X(02).
                 07 ROTSAI-6408-DATA-VENCTO PIC      X(10).
                 07 ROTSAI-6408-SIT-CLI-RET PIC      X(20).
                 07 ROTSAI-6408-END-RET     PIC      X(70).
                 07 ROTSAI-6408-NUM-END-RET PIC      X(07).
                 07 ROTSAI-6408-COM-END-RET PIC      X(30).
                 07 ROTSAI-6408-BAI-RET     PIC      X(50).
                 07 ROTSAI-6408-CID-RET     PIC      X(50).
                 07 ROTSAI-6408-CEP-RET     PIC      9(05).
                 07 ROTSAI-6408-CEP-CPL-RET PIC      9(03).
                 07 ROTSAI-6408-EST-RET     PIC      X(02).
                 07 ROTSAI-6408-SETOR-RET   PIC      9(03).
                 07 ROTSAI-6408-RAMO-RET    PIC      9(03).
                 07 ROTSAI-6408-PORTE-RET   PIC      9(03).
                 07 ROTSAI-6408-CD-DOCTO    PIC      X(15).

