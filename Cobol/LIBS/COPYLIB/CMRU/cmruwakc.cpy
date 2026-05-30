      ******************************************************************
      * NOME BOOK : CMRUWAKC                                           *
      * DESCRICAO : INTERFACE DO SERVICO FUNCIONAL BATCH CMRU2AKT.     *
      *             CONSULTA EXISTENCIA FICHA EMA ANDAMENTO            *
      * DATA      : FEV/2015                                           *
      * AUTOR     : EDSON MATSUMOTO                                    *
      * EMPRESA   : BSI TECNOLOGIA                                     *
      * COMPONENTE: CMRU - CONVIVENCIA E MIGRACAO - CREDITO RURAL      *
      * TAMANHO   : CMRUWAKC-TAM-LAYOUT                                *
      ******************************************************************
      * DATA       AUTOR       DESCRICAO / MANUTENCAO                  *
      ******************************************************************
      * XXXXXXXXXX XXXXXXXXX   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         *
      ******************************************************************
      * BLOCO DE IDENTIFICACAO                                         *
      * CMRUWAKC-COD-LAYOUT         = CODIGO DESTE LAYOUT              *
      * CMRUWAKC-TAM-LAYOUT         = TAMANHO DO REGISTRO              *
      *                                                                *
      * BLOCO DE ENTRADA                                               *
      * CMRUWAKC-E-CCLUB            = CODIGO DO CLUBE                  *
      *                                                                *
      * BLOCO DE SAIDA                                                 *
      * CMRUWAKC-S-CINDCD-FICHA     = INDICADOR DE FICHA PENDENTE      *
      *                                0 = SEM FICHA PENDENTE          *
      *                                1 = COM FICHA PENDENTE          *
      * CMRUWAKC-S-SQLCA            = AREA SQLC  P/ TRATAM.CHAMADOR    *
      * CMRUWAKC-S-ERRO-AREA        = AREA ERRO-AREA P/ TRATAM.CHAM    *
      *                                                                *
      ******************************************************************
           05 CMRUWAKC-HEADER.
              10 CMRUWAKC-COD-LAYOUT      PIC X(008) VALUE 'CMRUWAKC'.
              10 CMRUWAKC-TAM-LAYOUT      PIC 9(005) VALUE      00268.
           05 CMRUWAKC-REGISTRO.
              10 CMRUWAKC-BLOCO-RETORNO.
                 15 CMRUWAKC-COD-RETORNO           PIC  9(002).
                 15 CMRUWAKC-COD-ERRO              PIC  X(004).
                 15 CMRUWAKC-COD-MENSAGEM          PIC  X(008).
              10 CMRUWAKC-BLOCO-ENTRADA.
                 15 CMRUWAKC-E-CCLUB               PIC  9(010).
              10 CMRUWAKC-BLOCO-SAIDA.
                 15 CMRUWAKC-S-CINDCD-FICHA        PIC  9(001).
                 15 CMRUWAKC-S-SQLCA               PIC  X(136).
                 15 CMRUWAKC-S-ERRO-AREA           PIC  X(107).

