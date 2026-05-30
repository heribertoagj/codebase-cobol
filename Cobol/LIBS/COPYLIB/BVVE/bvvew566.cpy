      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW566                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO VVVE3566             *
      * DATA      : 08/09/2020                                         *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00766 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      * BVVEW566-E-SENTIDO-PAGINACAO:  'A' - ANTERIOR                  *
      *                                'I' - INICIAL                   *
      *                                'P' - PROXIMO                   *
      * BVVEW566-E-FLUXO-MSGEM     :   FLUXO DA MENSAGEM               *
      *                                'E' - ENVIADO                   *
      *                                'R' - RECEBIDO                  *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW566-REGISTRO.
              10 BVVEW566-BLOCO-ENTRADA.
                 15 BVVEW566-E-SENTIDO-PAGINACAO  PIC  X(001).
                 15 BVVEW566-E-DATA-CONSULTA      PIC  X(010).
                 15 BVVEW566-E-FLUXO-MSGEM        PIC  X(001).
                 15 BVVEW566-E-CMNTR-MSGRA        PIC  X(023).
               10 BVVEW566-BLOCO-RESTART.
                   15 BVVEW566-CHAVE-INI          PIC  9(004).
                   15 BVVEW566-CHAVE-FIM          PIC  9(004).
              10 BVVEW566-BLOCO-SAIDA.
                 15 BVVEW566-S-QTDE-CONSULTA      PIC  9(009).
                 15 BVVEW566-S-MAIS-REGISTROS     PIC  X(001).
                 15 BVVEW566-S-DADOS-CONSULTA     OCCURS 08 TIMES.
                    20 BVVEW566-S-XML-MENSAGEM    PIC  X(079).
              10 BVVEW566-BLOCO-RETORNO.
                 15 BVVEW566-COD-RET              PIC  9(002).
                 15 BVVEW566-MSG-RET              PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
