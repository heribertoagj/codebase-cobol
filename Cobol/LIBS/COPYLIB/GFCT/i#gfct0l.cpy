      *****************************************************************
      * NOME DA INC - I#GFCT0L                                        *
      * DESCRICAO   - MANUTENCAO TELAS GFCT                           *
      *               COMUNICACAO WEB X IMS - SBAT3000 - SAIDA        *
      * TAMANHO     - 1018                                            *
      * DATA        - 22.10.2004                                      *
      * RESPONSAVEL - MAURICIO - PROCWORK                             *
      *****************************************************************
       01  GFCT0L-AREA.
      *****************************************************************
      * DEFINE A AREA DE CONTROLE DE RETORNO PARA WEB                 *
      * ESTA AREA E ATUALIZADA, SOMENTE, PELO ROTEADOR                *
      *                                                               *
      * TPOMSG-WEB     - "HDER", "MSGI", "MSGF", "RSTA", "ERRO"       *
      * MPARCODE-APLIC - 00 - PROCESSAMENTO BEM SUCEDIDA              *
      *                  01 - ARGUMENTO NAO ENCONTRADO                *
      *                  02 - ERROS                                   *
      * NRO-MSGI       - CONTROLA QUANTIDADE DE MENSAGEM MSGI         *
      *                  ENVIADA AO WEB POR CICLO                     *
      *****************************************************************
           03  GFCT0L-RET-WEB.
               05 GFCT0L-TPOMSG-WEB                 PIC  X(004).
               05 GFCT0L-MPARCODE-APLIC             PIC  X(002).
               05 GFCT0L-NRO-MSGI                   PIC  9(002).
      *****************************************************************
      * DEFINE AREA DE ENTRADA DA MENSAGEM ORIGINADA DA WEB.          *
      *                                                               *
      * TAMANHO = 1010 BYTES (MAXIMO).                                *
      *****************************************************************
           03  GFCT0L-ENTRADA.
               05  GFCT0L-LL                        PIC S9(004) COMP.
               05  GFCT0L-ZZ                        PIC S9(004) COMP.
               05  GFCT0L-TRANSACAO                 PIC  X(009).
               05  GFCT0L-FUNCAO                    PIC  X(008).
               05  GFCT0L-DADOS                     PIC  X(989).
