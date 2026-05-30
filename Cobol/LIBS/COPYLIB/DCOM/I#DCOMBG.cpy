      **--------------------------------------------------------------**
      ** SISTEMA   : SNEB - NEGOCIACAO ELETRONICA    NOME: I#SNEBCD   **
      ** CRIACAO   : 31/05/2010                 ALTERACAO: DD/MM/AAAA **
      ** DESCRICAO : CONSULTA TAXA CUSTO BASE ANO/DIAS UTEIS          **
      **                                                              **
      ** TAMANHOS  : ENTRADA -  028                                   **
      **             SAIDA   -  247                                   **
      **                                                              **
      ** UTILIZACAO: SNEB8200                                         **
      ** COMENTARIO: AREA DE COMUNICACAO                              **
      **--------------------------------------------------------------**
       01  SNCD-ENTRADA.
           03  SNCD-CHAVE-PESQUISA.
               05  SNCD-CEMPR-INC                  PIC  9(005).
               05  SNCD-CINDIC-CURVA-CUSTO         PIC  9(003).
               05  SNCD-DPROJC-INDIC-CUSTO         PIC  X(010).
               05  SNCD-DVERTC-INDIC-CUSTO         PIC  X(010).
       01  SNCD-SAIDA.
           03  SNCD-TAMANHO-SAIDA                  PIC  9(005).
           03  SNCD-CODIGO-RETORNO                 PIC  9(002).
           03  SNCD-AVISO-RETORNO                  PIC  X(060).
           03  SNCD-DADOS.
               05  SNCD-RINDIC-CURVA-CUSTO         PIC  X(040).
               05  SNCD-SFATOR-DIA-TX              PIC  X(001).
               05  SNCD-NFATOR-DIA-TX              PIC  9(003)V9(015).
               05  SNCD-SFATOR-ACUM-TX             PIC  X(001).
               05  SNCD-NFATOR-ACUM-TX             PIC  9(003)V9(015).
               05  SNCD-SFATOR-DIA-ATIVO           PIC  X(001).
               05  SNCD-NFATOR-DIA-ATIVO           PIC  9(003)V9(015).
               05  SNCD-SFATOR-ACUM-ATIVO          PIC  X(001).
               05  SNCD-NFATOR-ACUM-ATIVO          PIC  9(003)V9(015).
               05  SNCD-SFATOR-DIA-PSIVO           PIC  X(001).
               05  SNCD-NFATOR-DIA-PSIVO           PIC  9(003)V9(015).
               05  SNCD-SFATOR-ACUM-PSIVO          PIC  X(001).
               05  SNCD-NFATOR-ACUM-PSIVO          PIC  9(003)V9(015).
               05  SNCD-STAXA-CALC-ANO             PIC  X(001).
               05  SNCD-NTAXA-CALC-ANO             PIC  9(003)V9(015).
               05  SNCD-QDIA-UTIL-VERTC            PIC  9(007).
      **                                                              **
      **--------------------------------------------------------------**

