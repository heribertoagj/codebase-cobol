      *****************************************************************
      * NOME DA INC - GFCTWAN7                                        *
      * DESCRICAO   - MODULO - VALIDAR NUMERO CELULAR P PERMITIR      *
      *               UTILIZACAO DO CELULAR NA ADESAO A CESTA CEL.    *
      * TAMANHO     - 273                                             *
      * DATA        - 26.07.2010                                      *
      * RESPONSAVEL - PAGNOCCA - PROCWORK                             *
      *                                                               *
      *                                                               *
      * COD-RETORNO:                                                  *
      *  00-NUM. CELULAR PASSIVEL DE UTILIZACAO                       *
      *  04-DADOS REPASSADOS GFCT5307 INCOSISTENTES                   *
      *  07-ERRO ACESSO GFCTB009                                      *
      *  09-ERRO ACESSO GFCTB0A1                                      *
      *  11-ERRO ACESSO GFCTB0C6                                      *
      *  13-ERRO ACESSO GFCTB0M2                                      *
      *  15-CELULAR VIGENTE EM OUTRA AG/CTA                           *
      *  17-ERRO NA LOGICA DO PROCESSAMENTO. NECESSARIO ANALISE       *
      *                                                               *
      *****************************************************************
      *
       01  GFCTWAN7-REG.
           05  WAN7-ENTRADA.
               07  WAN7-AMBIENTE              PIC  X(001).
               07  WAN7-CD-DDD                PIC  9(003).
               07  WAN7-NR-TELEFONE           PIC  9(011).
           05  WAN7-SAIDA.
               07  WAN7-COD-RETORNO           PIC  9(002).
               07  WAN7-DT-PROCM              PIC  X(010).
      *
      *------- DADOS OBTIDOS VIA CELULAR INFORMADO (M2 -> B9)
      *------- PODERAH RETORNAR COMO CANCELADO OU NAO (TP-CANCEL)
      *
               07  WAN7-DADOS-CEL.
                   10  WAN7-AGENCIA-CEL       PIC  9(005).
                   10  WAN7-CONTA-CEL         PIC  9(007).
                   10  WAN7-TARIFA-CEL        PIC  9(004).
                   10  WAN7-INC-TAR-CEL       PIC  X(026).
                   10  WAN7-INI-TAR-CEL       PIC  X(010).
                   10  WAN7-FIM-TAR-CEL       PIC  X(010).
                   10  WAN7-TP-CANCEL         PIC  9(001).
                   10  WAN7-NR-SEQ-CEL        PIC  9(005).
                   10  WAN7-CD-OPER-CEL       PIC  9(009).
      *
      *------- AREA INFORMANDO OCORRENCIA DE ERRO NAS BASES
      *
           05  WAN7-DADOS-ERRO.
               07  WAN7-OCORR-ERRO            PIC  X(001).
               07  WAN7-SQLCODE               PIC  9(003).
               07  WAN7-NOME-TAB              PIC  X(018).
               07  WAN7-COMANDO-SQL           PIC  X(010).
               07  WAN7-LOCAL                 PIC  9(004).
               07  WAN7-SQLCA                 PIC  X(133).
      *
