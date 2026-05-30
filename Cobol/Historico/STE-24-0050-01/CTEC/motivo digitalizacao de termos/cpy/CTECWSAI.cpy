      ******************************************************************
      * NOME BOOK : CTECWSAI - AREA DE COMUNICACAO DO PROGRAMA         *
      *             SARH9000 - OBTER NOME DE USUARIO                   *
      * DESCRICAO : AREA DE USO DO COORDENADOR                         *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: CTEC                                               *
      * TAMANHO   : 2031 BYTES                                         *
      ******************************************************************
      ******************************************************************
      * E-SISTEMA             = SISTEMA CHAMADOR                       *
      * E-PESQUISA            = CHAVE DE PESQUISA - CODIGO USUARIO     *
      * E-TAMANHO             = TAMANHO TOTAL DA AREA DE RETORNO       *
      * E-CAMPO               = QTDE CAMPOS SOLICITADOS RETORNO        *
      ******************************************************************
      * S-COD-RETORNO         = CODIGO RETORNO                         *
      * S-TAMANHO             = TAMANHO TOTAL DOS CAMPOS SOLICITAD.    *
      * S-ERRO-AREA           = AREA DE ERRO                           *
      * S-SQLCA               = AREA SQLCA                             *
      * S-REGISTRO            = REGISTRO DE SAIDA                      *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    *
      ******************************************************************
          05 CTECWSAI-AREA.
             10 CTECWSAI-REGISTRO.
                15 CTECWSAI-BLOCO-ENTRADA.
                   20 CTECWSAI-E-SISTEMA        PIC  X(004)
                                                     VALUE SPACES.
                   20 CTECWSAI-E-PESQUISA       PIC  9(009) VALUE ZEROS.
                   20 CTECWSAI-E-TAMANHO        PIC  9(004) VALUE ZEROS.
                   20 CTECWSAI-E-SOLICITA.
                      25 FILLER OCCURS 255 TIMES.
                         30 CTECWSAI-E-CAMPO    PIC  9(003).
                15 CTECWSAI-BLOCO-SAIDA.
                   20 CTECWSAI-S-COD-RETORNO    PIC  9(002) VALUE ZEROS.
                   20 CTECWSAI-S-TAMANHO        PIC  9(004) VALUE ZEROS.
                   20 CTECWSAI-S-ERRO-AREA      PIC  X(107)
                                                     VALUE SPACES.
                   20 CTECWSAI-S-SQLCA          PIC  X(136)
                                                     VALUE SPACES.
                   20 CTECWSAI-S-REGISTRO.
                      25 FILLER OCCURS 0 TO 1000 TIMES DEPENDING ON
                         CTECWSAI-S-TAMANHO.
                         30 CTECWSAI-S-CONTADOR PIC  X(01).


