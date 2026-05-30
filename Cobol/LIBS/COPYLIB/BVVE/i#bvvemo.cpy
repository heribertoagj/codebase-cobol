      ******************************************************************
      * NOME BOOK.: I#BVVEMO - INTERFACE FUNCIONAL/ACESSO A DADOS      *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO.: CONSULTAR CREDENCIADORA FUNCIONAL                  *
      * DATA......: MARCO/2021                                         *
      * AUTOR.....: FERNANDO LUIZ DE SANTI - WIPRO                     *
      * TAMANHO...: 0215 BYTES                                         *
      * ---------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      * ---------------------------------------------------------------*
      * BVVEMO-E-COD-PARM..........: COD PARAMETRO DESEJADO            *
      * BVVEMO-E-RESERVA...........: CAMPO RESERVA (ENVIAR BRANCOS)    *
      * BVVEMO-S-CONTEUDO-PARM.....: CONTEUDO PARAMETRO                *
      ******************************************************************
           05 BVVEMO-HEADER.
              10 BVVEMO-COD-LAYOUT      PIC  X(08)    VALUE 'BVVEMO'.
              10 BVVEMO-TAM-LAYOUT      PIC  9(05)    VALUE 0215.
           05 BVVEMO-REGISTRO.
              10 BVVEWAAE-BLOCO-ENTRADA.
                 15 BVVEMO-E-COD-PARM      PIC 9(003).
                 15 BVVEMO-E-RESERVA       PIC X(067).
              10 BVVEMO-BLOCO-SAIDA.
                 15 BVVEMO-S-COD-RETORNO   PIC 9(002).
                 15 BVVEMO-S-CONTEUDO-PARM PIC X(060).
                 15 BVVEMO-S-MENSAGEM      PIC X(070).
