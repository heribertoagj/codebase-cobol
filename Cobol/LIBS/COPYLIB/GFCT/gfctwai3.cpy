      *****************************************************************
      * NOME DA INC - GFCTWAI3                                        *
      * DESCRICAO   - MODULO ADESAO PACOTES CONTAS PAN - SAIDA        *
      * TAMANHO     - 170                                             *
      * DATA        - 21.08.2009                                      *
      * RESPONSAVEL - RICARDO PEREIRA - PROCWORK - TEL 45062345       *
      *****************************************************************
      *
       01  GFCTWAI3-SAIDA.
           03  GFCTWAI3-LL                            PIC S9(004) COMP.
           03  GFCTWAI3-ZZ                            PIC S9(004) COMP.
           03  GFCTWAI3-TRANSACAO                     PIC  X(009).
           03  GFCTWAI3-FUNCAO                        PIC  X(008).
           03  GFCTWAI3-DADOS-SAIDA.
               05  GFCTWAI3-COD-RETORNO               PIC  9(003).
      *            000 = OK
      *            001 = INCONSISTENTE
      *            002 = OK - ADESAO CTA COM DATA ABERTURA MES ANTERIOR
               05  GFCTWAI3-COD-MSG-GFCT.
                   07  GFCTWAI3-COD-MSG-RETORNO       PIC  9(004).
      *                CODIGO GFCT MENSAGEM DE RETORNO
                   07  GFCTWAI3-TXT-MSG-RETORNO       PIC  X(075).
      *                TEXTO  GFCT DA MENSAGEM DE RETORNO
               05  GFCTWAI3-CHAVE-LOG.
                   07  GFCTWAI3-LOG-TIMESTAMP         PIC  X(026).
                   07  GFCTWAI3-LOG-TRANSACAO         PIC  X(008).
                   07  GFCTWAI3-LOG-FUNCAO            PIC  X(008).
      *            CHAVE REG DE LOG EM CASO DE :
      *              COD RETORNO = 001 - INCONSISTENTE
      *              COD RETORNO = 002 - OK-DT ABERTURA CTA MES ANTERIOR
               05  FILLER                           PIC  X(025).
