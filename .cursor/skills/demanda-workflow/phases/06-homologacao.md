# Fase 6 — Homologacao (humana — gestor)

**Responsavel:** usuario / gestor de negocio.

**Pre-requisito:** Fase 5b concluida (subida MF, compile OK). O gestor **nao** consegue testar antes da subida — padrao cliente.

O agente **nao altera fontes** nesta fase. Aguardar informacao do usuario sobre resultado dos testes do gestor.

## Passos (usuario)

1. Gestor executa cenarios funcionais no ambiente MF (pos-promocao).
2. Informar ao agente: aprovado, reprovado ou pendencias.
3. Se **aprovado**: implantar PRD (pacote subida + contingencia conforme Fase 5b).
4. Se **reprovado**: corrigir fontes (Fase 4), repetir pre-subida e subida.

## Atualizar README quando usuario informar

| Momento | `## Status workflow` | Checkboxes |
|---|---|---|
| Gestor em testes (pos-subida) | `Homologacao — gestor em testes` | `[x] Pronto para subida`, `[x] Em homologacao` |
| Gestor aprovou — aguardando PRD | `Homologacao aprovada — aguardando implantacao PRD` | idem |
| Implantado em PRD | `Implantado` | `[x] Implantado` |

Checkboxes de referencia (gestor em testes):

```markdown
- [ ] Em andamento
- [ ] Em teste (dev interno — pre-subida)
- [x] Pronto para subida
- [x] Em homologacao
- [ ] Implantado
```

**Proximo passo apos OK do gestor:** implantacao PRD (humano), depois `Finalizar <ID>`.
