      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMX4 *
      * CRIACAO   : 06/2006                                   *
      * DESCRICAO : SELECAO DE CORRENTISTAS NA ROTINA CADU    *
      *                                                       *
      *           - SE FOR INFORMADO UM CPF/CNPJ, SERÙ        *
      *             RETORNADA UMA LISTA DE CONTAS-CORRENTES   *
      *             RELACIONADAS ∆QUELE CPF/CNPJ.             *
      *                                                       *
      *           - SE FOREM INFORMADAS AG®NCIA E CONTA, SERÙ *
      *             RETORNADA UMA LISTA COM OS CPFs/CNPJs   *
      *             RELACIONADOS ∆QUELA CONTA.                *
      *                                                       *
      * APLICACAO : MODULO DCOM6430                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * ÙREA DE ENTRADA                                       *
      *********************************************************

       01  ROTENT-6430-AREA.
           03 ROTENT-6430-ENVIO.
              05 ROTENT-6430-COD-ENV        PIC  X(04).
      *      INFORMAR ZEROS
              05 ROTENT-6430-MSG-ENV        PIC  X(79).
      *      INFORMAR BRANCOS
              05 ROTENT-6430-REST-ENV       PIC  9(05).
      *      INFORMAR NUMERO DE REGISTROS PARA RESTART
              05 ROTENT-6430-FLAG-ENV       PIC  X(01).
      *      INFORMAR N
              05 ROTENT-6430-START-ENV      PIC  X(01).
      *      INFORMAR N NA PRIMEIRA CHAMADA E S A PARTIR DA SEGUNDA
              05 ROTENT-6430-CFUNC-ENV      PIC  9(09).
      *      INFORMAR CODIGO DO FUNCIONARIO
              05 ROTENT-6430-CTERM-ENV      PIC  X(08).
      *      INFORMAR TERMINAL

              05 ROTENT-6430-CJUNC-ENV      PIC  9(05).
              05 ROTENT-6430-CTAC-ENV.
                 10 ROTENT-6430-CCTAC-ENV   PIC  9(07).
                 10 ROTENT-6430-DCTAC-ENV   PIC  9(01).
              05 ROTENT-6430-NUM-CNPJ-ENV   PIC  9(09).
              05 ROTENT-6430-FIL-CNPJ-ENV   PIC  9(04).
              05 ROTENT-6430-CTR-CNPJ-ENV   PIC  9(02).
              05 ROTENT-6430-CCLUB-ENV      PIC  9(10).

      *********************************************************
      * ÙREA DE RETORNO                                       *
      *********************************************************

           03 ROTSAI-6430-RETORNO.
              05 ROTSAI-6430-COD-RET       PIC  X(04).
      *
      *            00 - REGISTRO ENCONTRADO
      *            02 - RESTART
      *            03 - NENHUM REGISTRO ENCONTRADO NO CADU
      *            99 - PROBLEMAS DB2
      *           100  DADOS INCONSISTENTES
      *
              05 ROTSAI-6430-MENS-RET      PIC  X(79).
              05 ROTSAI-6430-REST-RET      PIC  9(05).
              05 ROTSAI-6430-FLAG-RET      PIC  X(01).
              05 ROTSAI-6430-QTDE-RET      PIC  9(03).
              05 ROTSAI-6430-COUNT-RET     PIC  9(09).
      *
              05 ROTSAI-TABELA-SAIDA     OCCURS    50  TIMES.
                 07 ROTSAI-6430-NM-RZSOC-RET PIC  X(70).
                 07 ROTSAI-6430-CCLUB-RET   PIC  9(10).
                 07 ROTSAI-6430-CNPJ-CADU.
                    10 ROTSAI-CCNPJ-CPF      PIC  9(09).
                    10 ROTSAI-CFLIAL-CNPJ    PIC  9(04).
                    10 ROTSAI-CCTRL-CNPJ-CPF PIC  9(02).
                 07 ROTSAI-6430-TP-PESSOA   PIC  X(01).
                 07 ROTSAI-6430-TIT-CLI-RET PIC  9(02).
                 07 ROTSAI-6430-DATA-VENCTO PIC  X(10).
                 07 ROTSAI-6430-SIT-OPE-RET PIC  X(20).
                 07 ROTSAI-6430-CJUNC-RET   PIC  9(05).
                 07 ROTSAI-6430-CTAC-RET.
                    10 ROTSAI-6430-CCTAC-RET PIC 9(07).
                    10 ROTSAI-6430-DCTAC-RET PIC 9(01).
                 07 ROTSAI-6430-NOME-AGE    PIC  X(30).
