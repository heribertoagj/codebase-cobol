      ******************************************************************
      * NOME BOOK : I#DCOMZF                                           *
      * DESCRI#AO : BOOK DE INTERFACE PARA CONSULTAR AGENDA DO CLIENTE *
      * DATA      : 08/2020                                            *
      * AUTOR     : DANIELA GALVAO - CAPGEMINI - ALPHA                 *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : DCOM - DESCONTO COMERCIAL                          *
      * TAMANHO   : 0148 BYTES                                         *
      ***************************DESCRICAO******************************
      * DCOMZF-CCUSTO.........: CENTRO DE CUSTO                        *
      * DCOMZF-DT-MOVTO.......: DATA DO MOVIMENTO                      *
      * DCOMZF-MIDIA..........: MIDIA                                  *
      * DCOMZF-BANCO..........: BANCO                                  *
      * DCOMZF-AGENCIA........: AGENCIA                                *
      * DCOMZF-CONTA..........: CONTA                                  *
      * DCOMZF-PRODUTO........: BANDEIRA                               *
      * DCOMZF-MSG-DESP: MENSAGEM DE ERRO                              *
      *----------------------------------------------------------------*
      * OS CAMPOS ABAIXO SERAO PREENCHIDOS SOMENTE NA BAIXA DSA TABELA *
      * DCOMZF-TP-VLR-OU-PERC.: TIPO - V - VALOR                       *
      *                                P - PERCENTUAL                  *
      * DCOMZF-VALOR-OU-PERC..: VALOR OU PERCENTUAL DO ADITAMENTO      *
      *----------------------------------------------------------------*
      * OS CAMPOS ABAIXO SERAO PREENCHIDOS SOMENTE APOS O BATIMENTO COM*
      * AS AGENDAS ENVIADAS PELO BVVE.                                 *
      * DCOMZF-DT-UR..........: DATA DA ULTIMA AGENDA SELECIONADA      *
      * DCOMZF-VALOR-PRODUTO..: VALOR DEFINIDO PARA O PRODUTO
      ******************************************************************
          05 DCOMZF-REGISTRO.
             10 DCOMZF-CCUSTO                   PIC  X(04).
             10 DCOMZF-DT-MOVTO                 PIC  X(10).
             10 DCOMZF-MIDIA                    PIC  9(03).
             10 DCOMZF-BANCO                    PIC  9(03).
             10 DCOMZF-AGENCIA                  PIC  9(05).
             10 DCOMZF-CONTA                    PIC  9(13).
             10 DCOMZF-PRODUTO                  PIC  9(05).
             10 DCOMZF-TP-VLR-OU-PERC           PIC  X(01).
             10 DCOMZF-VALOR-OU-PERC            PIC  9(15)V9(02).
             10 DCOMZF-MSG-DESP                 PIC  X(40).
             10 DCOMZF-DT-UR                    PIC  X(10).
             10 DCOMZF-VALOR-PRODUTO            PIC  9(15)V9(02) COMP-3.
             10 FILLER                          PIC  X(180).
