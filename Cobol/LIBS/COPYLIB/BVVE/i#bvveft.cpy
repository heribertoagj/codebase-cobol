      ******************************************************************
      *  NOVOS NORMATIVOS - RESOLUCAO 4734                             *
      *                                                                *
      ******************************************************************
      *                                                                *
      *  NOME DO BOOK.: I#BVVEFT - AREA DO MODULO BVVEA518             *
      *  DESCRICAO....: SOLICITACAO DE RENEGOCIACAO                    *
      *  TAMANHO......: 00551 BYTES                                    *
      *                                                                *
      *--ENTRADA-------------------------------------------------------*
      *                                                                *
      *  CCUSTO.......: CENTRO DE CUSTO                                *
      *  CONTRATO.....: NUMERO DO CONTRATO GRAN                        *
      *  FIM-OCORR....: INDICA FINAL DAS OCORRENCIAS / ULTIMA CHAMADA  *
      *                                                                *
      *  OCORRENCIAS                                                   *
      *   CONTR-NEG....: CONTRATO PARA RENEGOCIACAO                    *
      *                                                                *
      *--SAIDA---------------------------------------------------------*
      *                                                                *
      *  COD-RETORNO...: CODIGO DE RETORNO                             *
      *  MSG-RETORNO...: MENSGEM DE RETORNO                            *
      *                                                                *
      ******************************************************************
      *                                                                *
       01  BVVEFT-COMMAREA.
           05 BVVEFT-AREA-ENTRADA.
              10 BVVEFT-E-CCUSTO                       PIC  X(004).
              10 BVVEFT-E-CONTRATO                     PIC  X(019).
              10 BVVEFT-E-FIM-OCORR                    PIC  X(001).

              10 BVVEFT-E-OCORRENCIAS   OCCURS 25 TIMES.
                 15 BVVEFT-E-CONTR-NEG                 PIC  X(019).

           05 BVVEFT-AREA-SAIDA.
              10 BVVEFT-S-COD-RETORNO                  PIC  9(002).
              10 BVVEFT-S-MSG-RETORNO                  PIC  X(050).
