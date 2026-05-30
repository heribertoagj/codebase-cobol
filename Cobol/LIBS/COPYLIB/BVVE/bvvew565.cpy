      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW565                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3565             *
      * DATA      : 07/09/2020                                         *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00766 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      * BVVEW565-E-TIPO-ACESSO      :  1   - OBTEM O TXT INTEIRO       *
      *                             :  2   - OBTEM 8 LINHAS PARA EXIBIR*
      *                                      NA TELA                   *
      * BVVEW565-E-SENTIDO-PAGINACAO:  'A' - ANTERIOR                  *
      *                                'I' - INICIAL                   *
      *                                'P' - PROXIMO                   *
      * BVVEW565-E-FLUXO-MSGEM     :   FLUXO DA MENSAGEM               *
      *                                'E' - ENVIADO                   *
      *                                'R' - RECEBIDO                  *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW565-REGISTRO.
              10 BVVEW565-BLOCO-ENTRADA.
                 15 BVVEW565-E-TIPO-ACESSO        PIC  9(001).
                 15 BVVEW565-E-SENTIDO-PAGINACAO  PIC  X(001).
                 15 BVVEW565-E-DATA-CONSULTA      PIC  X(010).
                 15 BVVEW565-E-FLUXO-MSGEM        PIC  X(001).
                 15 BVVEW565-E-CMNTR-MSGRA        PIC  X(023).
               10 BVVEW565-BLOCO-RESTART.
                   15 BVVEW565-CHAVE-INI          PIC  9(004).
                   15 BVVEW565-CHAVE-FIM          PIC  9(004).
              10 BVVEW565-BLOCO-SAIDA.
                 15 BVVEW565-S-QTDE-CONSULTA      PIC  9(009).
                 15 BVVEW565-S-MAIS-REGISTROS     PIC  X(001).
                 15 BVVEW565-S-DADOS-CONSULTA     OCCURS 08 TIMES.
                    20 BVVEW565-S-TXT-MENSAGEM    PIC  X(079).
                 15 FILLER                        PIC  X(31233).
              10 BVVEW565-BLOCO-SAIDA-2 REDEFINES BVVEW565-BLOCO-SAIDA.
                 15 BVVEW565-S-WCONTD-MSGEM-TEXTO PIC  X(31875).
              10 BVVEW565-BLOCO-RETORNO.
                 15 BVVEW565-COD-RET              PIC  9(002).
                 15 BVVEW565-MSG-RET              PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
